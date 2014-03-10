import os
import errno
import fnmatch
import stat

try:
    from . import msg, utils
    assert msg and utils
except ImportError:
    import msg

try:
    unicode()
except NameError:
    unicode = str


IGNORE_FILES = ['.gitignore', '.hgignore', '.flignore', '.flooignore']
# TODO: make this configurable
HIDDEN_WHITELIST = ['.floo'] + IGNORE_FILES
# TODO: grab global git ignores:
# gitconfig_file = popen("git config -z --get core.excludesfile", "r");
DEFAULT_IGNORES = ['extern', 'node_modules', 'tmp', 'vendor']
MAX_FILE_SIZE = 1024 * 1024 * 5


def create_flooignore(path):
    flooignore = os.path.join(path, '.flooignore')
    # A very short race condition, but whatever.
    if os.path.exists(flooignore):
        return
    try:
        with open(flooignore, 'w') as fd:
            fd.write('\n'.join(DEFAULT_IGNORES))
    except Exception as e:
        msg.error('Error creating default .flooignore: %s' % str(e))


class Ignore(object):
    def __init__(self, parent, path, recurse=True):
        self.parent = parent
        self.size = 0
        self.children = []
        self.files = []
        self.ignores = {
            '/TOO_BIG/': []
        }
        self.path = utils.unfuck_path(path)

        try:
            paths = os.listdir(self.path)
        except OSError as e:
            if e.errno != errno.ENOTDIR:
                msg.error('Error listing path %s: %s' % (path, unicode(e)))
                return
            self.path = os.path.dirname(self.path)
            self.add_file(os.path.basename(path))
            return
        except Exception as e:
            msg.error('Error listing path %s: %s' % (path, unicode(e)))
            return

        msg.debug('Initializing ignores for %s' % path)
        for ignore_file in IGNORE_FILES:
            try:
                self.load(ignore_file)
            except:
                pass

        if recurse:
            for p in paths:
                self.add_file(p)

    def add_file(self, p):
        p_path = os.path.join(self.path, p)
        if p[0] == '.' and p not in HIDDEN_WHITELIST:
            msg.log('Ignoring hidden path %s' % p_path)
            return
        is_ignored = self.is_ignored(p_path)
        if is_ignored:
            msg.log(is_ignored)
            return
        try:
            s = os.stat(p_path)
        except Exception as e:
            msg.error('Error stat()ing path %s: %s' % (p_path, unicode(e)))
            return
        if stat.S_ISDIR(s.st_mode):
            ig = Ignore(self, p_path)
            self.children.append(ig)
            self.size += ig.size
            return
        elif stat.S_ISREG(s.st_mode):
            if s.st_size > (MAX_FILE_SIZE):
                self.ignores['/TOO_BIG/'].append(p)
                msg.log(self.is_ignored_message(p_path, p, '/TOO_BIG/'))
            else:
                self.size += s.st_size
                self.files.append(p)

    def load(self, ignore_file):
        with open(os.path.join(self.path, ignore_file), 'r') as fd:
            ignores = fd.read()
        self.ignores[ignore_file] = []
        for ignore in ignores.split('\n'):
            ignore = ignore.strip()
            if len(ignore) == 0:
                continue
            if ignore[0] == '#':
                continue
            msg.debug('Adding %s to ignore patterns' % ignore)
            self.ignores[ignore_file].append(ignore)

    def list_paths(self):
        for f in self.files:
            yield os.path.join(self.path, f)
        for c in self.children:
            for p in c.list_paths():
                yield p

    def is_ignored_message(self, path, pattern, ignore_file):
        if ignore_file == '/TOO_BIG/':
            return '%s ignored because it is too big (more than %s bytes)' % (path, MAX_FILE_SIZE)
        return '%s ignored by pattern %s in %s' % (path, pattern, os.path.join(self.path, ignore_file))

    def is_ignored(self, path, is_dir=None):
        rel_path = os.path.relpath(path, self.path)
        for ignore_file, patterns in self.ignores.items():
            for pattern in patterns:
                base_path, file_name = os.path.split(rel_path)
                if pattern[0] == '/':
                    # Only match immediate children
                    if utils.unfuck_path(base_path) == self.path and fnmatch.fnmatch(file_name, pattern[1:]):
                        return self.is_ignored_message(path, pattern, ignore_file)
                else:
                    if len(pattern) > 0 and pattern[-1] == '/':
                        if is_dir is None:
                            try:
                                s = os.stat(path)
                            except Exception as e:
                                msg.error('Error lstat()ing path %s: %s' % (path, unicode(e)))
                                continue
                            is_dir = stat.S_ISDIR(s.st_mode)
                        if is_dir:
                            pattern = pattern[:-1]
                    if fnmatch.fnmatch(file_name, pattern):
                        return self.is_ignored_message(path, pattern, ignore_file)
                    if fnmatch.fnmatch(rel_path, pattern):
                        return self.is_ignored_message(path, pattern, ignore_file)
        if self.parent:
            return self.parent.is_ignored(path)
        return False


def is_ignored(current_path, abs_path=None):
    abs_path = abs_path or current_path
    if not utils.is_shared(current_path):
        return True

    path = utils.to_rel_path(current_path)  # Never throws ValueError because is_shared would return False
    if path == ".":
        return False

    base_path, file_name = os.path.split(current_path)
    ig = Ignore(None, base_path, recurse=False)
    if ig.is_ignored(abs_path):
        return True

    return is_ignored(base_path, abs_path)
