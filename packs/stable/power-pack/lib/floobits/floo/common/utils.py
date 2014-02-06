import os
import json
import re
import hashlib
import webbrowser

from functools import wraps

try:
    from urllib.parse import urlparse
    assert urlparse
except ImportError:
    from urlparse import urlparse

try:
    from .. import editor
    from . import shared as G
    from . import msg
    from .lib import DMP
    assert G and DMP
except ImportError:
    import editor
    import msg
    import shared as G
    from lib import DMP


class FlooPatch(object):
    def __init__(self, current, buf):
        self.buf = buf
        self.current = current
        self.previous = buf['buf']
        if buf['encoding'] == 'base64':
            self.md5_before = hashlib.md5(self.previous).hexdigest()
        else:
            try:
                self.md5_before = hashlib.md5(self.previous.encode('utf-8')).hexdigest()
            except Exception:
                # Horrible fallback if for some reason encoding doesn't agree with actual object
                self.md5_before = hashlib.md5(self.previous).hexdigest()

    def __str__(self):
        return '%s - %s' % (self.buf['id'], self.buf['path'])

    def patches(self):
        return DMP.patch_make(self.previous, self.current)

    def to_json(self):
        patches = self.patches()
        if len(patches) == 0:
            return None
        patch_str = ''
        for patch in patches:
            patch_str += str(patch)

        if self.buf['encoding'] == 'base64':
            md5_after = hashlib.md5(self.current).hexdigest()
        else:
            md5_after = hashlib.md5(self.current.encode('utf-8')).hexdigest()

        return {
            'id': self.buf['id'],
            'md5_after': md5_after,
            'md5_before': self.md5_before,
            'path': self.buf['path'],
            'patch': patch_str,
            'name': 'patch'
        }


class Waterfall(object):
    def __init__(self):
        self.chain = []

    def add(self, f, *args, **kwargs):
        self.chain.append(lambda: f(*args, **kwargs))

    def call(self):
        res = [f() for f in self.chain]
        self.chain = []
        return res


def reload_settings():
    floorc_settings = load_floorc()
    for name, val in floorc_settings.items():
        setattr(G, name, val)
    if G.SHARE_DIR:
        G.BASE_DIR = G.SHARE_DIR
    G.BASE_DIR = os.path.realpath(os.path.expanduser(G.BASE_DIR))
    G.COLAB_DIR = os.path.join(G.BASE_DIR, 'share')
    G.COLAB_DIR = os.path.realpath(G.COLAB_DIR)
    mkdir(G.COLAB_DIR)


def load_floorc():
    """try to read settings out of the .floorc file"""
    s = {}
    try:
        fd = open(G.FLOORC_PATH, 'r')
    except IOError as e:
        if e.errno == 2:
            return s
        raise

    default_settings = fd.read().split('\n')
    fd.close()

    for setting in default_settings:
        # TODO: this is horrible
        if len(setting) == 0 or setting[0] == '#':
            continue
        try:
            name, value = setting.split(' ', 1)
        except IndexError:
            continue
        s[name.upper()] = value
    return s


cancelled_timeouts = set()
timeout_ids = set()


def set_timeout(func, timeout, *args, **kwargs):
    timeout_id = set_timeout._top_timeout_id
    if timeout_id > 100000:
        set_timeout._top_timeout_id = 0
    else:
        set_timeout._top_timeout_id += 1

    def timeout_func():
        timeout_ids.discard(timeout_id)
        if timeout_id in cancelled_timeouts:
            cancelled_timeouts.remove(timeout_id)
            return
        func(*args, **kwargs)
    editor.set_timeout(timeout_func, timeout)
    timeout_ids.add(timeout_id)
    return timeout_id

set_timeout._top_timeout_id = 0


def cancel_timeout(timeout_id):
    if timeout_id in timeout_ids:
        cancelled_timeouts.add(timeout_id)


def parse_url(workspace_url):
    secure = G.SECURE
    owner = None
    workspace_name = None
    parsed_url = urlparse(workspace_url)
    port = parsed_url.port
    if parsed_url.scheme == 'http':
        if not port:
            port = 3148
        secure = False
    else:
        if not port:
            port = G.DEFAULT_PORT
    result = re.match('^/([-\@\+\.\w]+)/([-\.\w]+)/?$', parsed_url.path)
    if not result:
        result = re.match('^/r/([-\@\+\.\w]+)/([-\.\w]+)/?$', parsed_url.path)

    if result:
        (owner, workspace_name) = result.groups()
    else:
        raise ValueError('%s is not a valid Floobits URL' % workspace_url)

    return {
        'host': parsed_url.hostname,
        'owner': owner,
        'port': port,
        'workspace': workspace_name,
        'secure': secure,
    }


def to_workspace_url(r):
    port = int(r.get('port', 3448))
    if r['secure']:
        proto = 'https'
        if port == 3448:
            port = ''
    else:
        proto = 'http'
        if port == 3148:
            port = ''
    if port != '':
        port = ':%s' % port
    host = r.get('host', G.DEFAULT_HOST)
    workspace_url = '%s://%s%s/%s/%s/' % (proto, host, port, r['owner'], r['workspace'])
    return workspace_url


def get_full_path(p):
    full_path = os.path.join(G.PROJECT_PATH, p)
    return unfuck_path(full_path)


def unfuck_path(p):
    return os.path.normcase(os.path.normpath(p))


def to_rel_path(p):
    return os.path.relpath(p, G.PROJECT_PATH).replace(os.sep, '/')


def to_scheme(secure):
    if secure is True:
        return 'https'
    return 'http'


def is_shared(p):
    if not G.JOINED_WORKSPACE:
        return False
    p = unfuck_path(p)
    try:
        if to_rel_path(p).find('../') == 0:
            return False
    except ValueError:
        return False
    return True


def update_floo_file(path, data):
    try:
        floo_json = json.loads(open(path, 'r').read())
    except Exception:
        pass

    try:
        floo_json.update(data)
    except Exception:
        floo_json = data

    with open(path, 'w') as floo_fd:
        floo_fd.write(json.dumps(floo_json, indent=4, sort_keys=True))


def get_persistent_data(per_path=None):
    per_data = {'recent_workspaces': [], 'workspaces': {}}
    per_path = per_path or os.path.join(G.BASE_DIR, 'persistent.json')
    try:
        per = open(per_path, 'rb')
    except (IOError, OSError):
        msg.debug('Failed to open %s. Recent workspace list will be empty.' % per_path)
        return per_data
    try:
        data = per.read().decode('utf-8')
        persistent_data = json.loads(data)
    except Exception as e:
        msg.debug('Failed to parse %s. Recent workspace list will be empty.' % per_path)
        msg.debug(str(e))
        msg.debug(data)
        return per_data
    if 'recent_workspaces' not in persistent_data:
        persistent_data['recent_workspaces'] = []
    if 'workspaces' not in persistent_data:
        persistent_data['workspaces'] = {}
    return persistent_data


def update_persistent_data(data):
    seen = set()
    data['recent_workspaces'] = [x for x in data['recent_workspaces'] if x['url'] not in seen and not seen.add(x['url'])]
    per_path = os.path.join(G.BASE_DIR, 'persistent.json')
    with open(per_path, 'wb') as per:
        per.write(json.dumps(data, indent=2).encode('utf-8'))


def add_workspace_to_persistent_json(owner, name, url, path):
    d = get_persistent_data()
    workspaces = d['workspaces']
    if owner not in workspaces:
        workspaces[owner] = {}
    workspaces[owner][name] = {'url': url, 'path': path}
    update_persistent_data(d)


def get_workspace_by_path(path):
    for owner, workspaces in get_persistent_data()['workspaces'].items():
        for name, workspace in workspaces.items():
            if workspace['path'] == path:
                workspace_url = workspace['url']
                return workspace_url


def rm(path):
    """removes path and dirs going up until a OSError"""
    os.remove(path)
    try:
        os.removedirs(os.path.split(path)[0])
    except OSError:
        pass


def mkdir(path):
    try:
        os.makedirs(path)
    except OSError as e:
        if e.errno != 17:
            editor.error_message('Cannot create directory {0}.\n{1}'.format(path, e))
            raise


def save_buf(buf):
    path = get_full_path(buf['path'])
    mkdir(os.path.split(path)[0])
    with open(path, 'wb') as fd:
        if buf['encoding'] == 'utf8':
            fd.write(buf['buf'].encode('utf-8'))
        else:
            fd.write(buf['buf'])


def _unwind_generator(gen_expr, cb=None, res=None):
    try:
        while True:
            arg0 = res
            args = []
            if type(res) == tuple:
                arg0 = res[0]
                args = list(res[1:])
            if not callable(arg0):
                # send only accepts one argument... this is slightly dangerous if
                # we ever just return a tuple of one elemetn
                if type(res) == tuple and len(res) == 1:
                    res = gen_expr.send(res[0])
                else:
                    res = gen_expr.send(res)
            else:
                def f(*args):
                    return _unwind_generator(gen_expr, cb, args)
                args.append(f)
                return arg0(*args)
        # TODO: probably shouldn't catch StopIteration to return since that can occur by accident...
    except StopIteration:
        pass
    except __StopUnwindingException as e:
        res = e.ret_val
    if cb:
        return cb(res)
    return res


class __StopUnwindingException(BaseException):
    def __init__(self, ret_val):
        self.ret_val = ret_val


def return_value(args):
    raise __StopUnwindingException(args)


def inlined_callbacks(f):
    """ Branching logic in async functions generates a callback nightmare.
    Use this decorator to inline the results.  If you yield a function, it must
    accept a callback as its final argument that it is responsible for firing.

    example usage:
    """
    @wraps(f)
    def wrap(*args, **kwargs):
        return _unwind_generator(f(*args, **kwargs))
    return wrap


def has_browser():
    valid_browsers = [
        "MacOSX",  # Default mac browser.
        "Chrome",
        "Chromium",
        "Firefox",
        "Safari",
        "Opera"
    ]
    for browser in valid_browsers:
        try:
            webbrowser.get(browser)
            return True
        except Exception:
            continue
    return False
