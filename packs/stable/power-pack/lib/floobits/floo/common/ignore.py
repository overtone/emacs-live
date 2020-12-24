import os
import errno
import fnmatch
import stat
import subprocess

try:
    from . import msg, utils
    from .exc_fmt import str_e
    assert msg and str_e and utils
except ImportError:
    import msg
    from exc_fmt import str_e

IGNORE_FILES = ['.gitignore', '.hgignore', '.ignore', '.flooignore']
HIDDEN_WHITELIST = ['.floo'] + IGNORE_FILES
BLACKLIST = [
    '.DS_Store',
    '.git/',
    '.svn/',
    '.hg/',
]

NEGATE_PREFIXES = ['!', '^']

DEFAULT_IGNORES = [
    '#*',
    '*.o',
    '*.pyc',
    '*.pyo',
    '*~',
    'extern/',
    'heroku.yml',
    'node_modules/',
    'tmp',
    'vendor/',
]
MAX_FILE_SIZE = 1024 * 1024 * 5

IS_IG_IGNORED = 1
IS_IG_CHECK_CHILD = 2


def get_git_excludesfile():
    global_ignore = None
    try:
        p = subprocess.Popen(['git', 'config -z --get core.excludesfile'], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        result = p.communicate()
        global_ignore = result[0]
        if not global_ignore:
            return
        global_ignore = os.path.realpath(os.path.expanduser(global_ignore.decode('utf-8')))
        msg.log('git core.excludesfile is ', global_ignore)
    except Exception as e:
        msg.error('Error getting git core.excludesfile:', str_e(e))
    return global_ignore


def create_flooignore(path):
    flooignore = os.path.join(path, '.flooignore')
    # A very short race condition, but whatever.
    if os.path.exists(flooignore):
        return
    try:
        with open(flooignore, 'w') as fd:
            fd.write('\n'.join(DEFAULT_IGNORES))
    except Exception as e:
        msg.error('Error creating default .flooignore: ', str_e(e))


def create_ignore_tree(path):
    create_flooignore(path)
    ig = Ignore(path)
    global_ignore = get_git_excludesfile()
    if global_ignore:
        ig.load(global_ignore)
    ig.ignores['/DEFAULT/'] = BLACKLIST
    ig.recurse(ig)
    return ig


class Ignore(object):
    def __init__(self, path, parent=None):
        self.parent = parent
        self.size = 0
        self.total_size = 0
        self.children = {}
        self.files = []
        self.ignores = {
            '/TOO_BIG/': []
        }
        self.path = utils.unfuck_path(path)

    def recurse(self, root):
        try:
            paths = os.listdir(self.path)
        except OSError as e:
            if e.errno != errno.ENOTDIR:
                msg.error('Error listing path ', self.path, ': ', str_e(e))
            return
        except Exception as e:
            msg.error('Error listing path ', self.path, ': ', str_e(e))
            return

        msg.debug('Initializing ignores for ', self.path)
        for ignore_file in IGNORE_FILES:
            try:
                self.load(ignore_file)
            except Exception:
                pass

        for p in paths:
            if p == '.' or p == '..':
                continue
            if p in BLACKLIST:
                msg.log('Ignoring blacklisted file ', p)
                continue
            p_path = os.path.join(self.path, p)
            try:
                s = os.stat(p_path)
            except Exception as e:
                msg.error('Error stat()ing path ', p_path, ': ', str_e(e))
                continue

            if stat.S_ISREG(s.st_mode) and p in HIDDEN_WHITELIST:
                # Don't count these whitelisted files in size
                self.files.append(p_path)
                continue

            is_dir = stat.S_ISDIR(s.st_mode)
            if root.is_ignored(p_path, is_dir, True):
                continue

            if is_dir:
                ig = Ignore(p_path, self)
                self.children[p] = ig
                ig.recurse(root)
                self.total_size += ig.total_size
                continue

            if stat.S_ISREG(s.st_mode):
                if s.st_size > (MAX_FILE_SIZE):
                    self.ignores['/TOO_BIG/'].append(p)
                    msg.log(self.is_ignored_message(p_path, p, '/TOO_BIG/', False))
                else:
                    self.size += s.st_size
                    self.total_size += s.st_size
                    self.files.append(p_path)

    def load(self, ignore_file):
        with open(os.path.join(self.path, ignore_file), 'r') as fd:
            ignores = fd.read()
        rules = []
        for ignore in ignores.split('\n'):
            ignore = ignore.strip()
            if len(ignore) == 0:
                continue
            if ignore[0] == '#':
                continue
            if ignore in NEGATE_PREFIXES:
                # Just an exclamation mark or caret? This is some messed up pattern. Skip it.
                continue
            msg.debug('Adding ', ignore, ' to ignore patterns')
            rules.insert(0, ignore)
        self.ignores[ignore_file] = rules

    def get_children(self):
        children = list(self.children.values())
        for c in self.children.values():
            children += c.get_children()
        return children

    def list_paths(self):
        for f in self.files:
            yield os.path.join(self.path, f)
        for c in self.children.values():
            for p in c.list_paths():
                yield p

    def is_ignored_message(self, rel_path, pattern, ignore_file, negate):
        path = os.path.join(self.path, rel_path)
        negate_msg = ''
        if negate:
            negate_msg = '__NOT__ '
        if ignore_file == '/TOO_BIG/':
            return '%s %signored because it is too big (more than %s bytes)' % (path, negate_msg, MAX_FILE_SIZE)
        return '%s %signored by pattern %s in %s' % (path, negate_msg, pattern, os.path.join(self.path, ignore_file))

    def is_ignored(self, path, is_dir=None, log=False):
        if is_dir is None:
            try:
                s = os.stat(path)
            except Exception as e:
                msg.error('Error lstat()ing path ', path, ': ', str_e(e))
                return True
            is_dir = stat.S_ISDIR(s.st_mode)
        rel_path = os.path.relpath(path, self.path).replace(os.sep, '/')
        return self._is_ignored(rel_path, is_dir, log)

    def _is_ignored(self, rel_path, is_dir, log):
        base_path, file_name = os.path.split(rel_path)

        if not is_dir and file_name in HIDDEN_WHITELIST:
            return False

        for ignore_file, patterns in self.ignores.items():
            for pattern in patterns:
                orig_pattern = pattern
                negate = False
                match = False
                if pattern[0] in NEGATE_PREFIXES:
                    negate = True
                    pattern = pattern[1:]

                if not pattern:
                    continue

                if pattern[0] == '/':
                    match = fnmatch.fnmatch(rel_path, pattern[1:])
                else:
                    if len(pattern) > 0 and pattern[-1] == '/':
                        if is_dir:
                            pattern = pattern[:-1]
                    if file_name == pattern:
                        match = True
                    elif base_path == pattern or (pattern[-1] == '/' and base_path == pattern[:-1]):
                        match = True
                    elif fnmatch.fnmatch(file_name, pattern):
                        match = True
                    elif fnmatch.fnmatch(rel_path, pattern):
                        match = True
                if match:
                    if log:
                        msg.log(self.is_ignored_message(rel_path, orig_pattern, ignore_file, negate))
                    if negate:
                        return False
                    return True

        split = rel_path.split("/", 1)
        if len(split) != 2:
            return False
        name, new_path = split
        ig = self.children.get(name)
        if ig:
            return ig._is_ignored(new_path, is_dir, log)
        return False
