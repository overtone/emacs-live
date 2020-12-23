import os
import errno
import json
import re
import hashlib
import time
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
    from .exc_fmt import str_e
    from . import msg
    from .lib import DMP
    assert G and DMP
except ImportError:
    import editor
    import msg
    from exc_fmt import str_e
    import shared as G
    from lib import DMP


class JOIN_ACTION(object):
    PROMPT = 1
    UPLOAD = 2
    DOWNLOAD = 3


class FlooPatch(object):
    def __init__(self, current, buf):
        self.buf = buf
        self.current = current
        self.previous = buf['buf']
        if buf['encoding'] == 'base64':
            self.md5_before = hashlib.md5(self.previous).hexdigest()
            self.md5_after = hashlib.md5(self.current).hexdigest()
        else:
            try:
                self.md5_before = hashlib.md5(self.previous.encode('utf-8')).hexdigest()
            except Exception as e:
                # Horrible fallback if for some reason encoding doesn't agree with actual object
                self.md5_before = hashlib.md5(self.previous).hexdigest()
                msg.log('Error calculating md5_before for ', str(self), ': ', str_e(e))
            try:
                self.md5_after = hashlib.md5(self.current.encode('utf-8')).hexdigest()
            except Exception as e:
                # Horrible fallback if for some reason encoding doesn't agree with actual object
                self.md5_after = hashlib.md5(self.current).hexdigest()
                msg.log('Error calculating md5_after for ', str(self), ': ', str_e(e))

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

        return {
            'id': self.buf['id'],
            'md5_after': self.md5_after,
            'md5_before': self.md5_before,
            'path': self.buf['path'],
            'patch': patch_str,
            'name': 'patch'
        }


def reload_settings():
    floorc_settings = load_floorc_json()
    for name, val in floorc_settings.items():
        setattr(G, name, val)
    validate_auth(G.AUTH)
    if G.SHARE_DIR:
        G.BASE_DIR = G.SHARE_DIR
    G.BASE_DIR = os.path.realpath(os.path.expanduser(G.BASE_DIR))
    G.COLAB_DIR = os.path.join(G.BASE_DIR, 'share')
    G.COLAB_DIR = os.path.realpath(G.COLAB_DIR)
    if G.DEBUG:
        msg.LOG_LEVEL = msg.LOG_LEVELS['DEBUG']
    else:
        msg.LOG_LEVEL = msg.LOG_LEVELS['MSG']
    mkdir(G.COLAB_DIR)
    return floorc_settings


def load_floorc_json():
    # Expose a few settings for curious users to tweak
    s = {
        'expert_mode': False,
        'debug': False,
    }
    try:
        with open(G.FLOORC_JSON_PATH, 'r') as fd:
            floorc_json = fd.read()
    except IOError as e:
        if e.errno == errno.ENOENT:
            return s
        raise

    try:
        default_settings = json.loads(floorc_json)
    except ValueError:
        return s

    for k, v in default_settings.items():
        s[k.upper()] = v
    return s


def save_floorc_json(s):
    floorc_json = {}
    for k, v in s.items():
        floorc_json[k.lower()] = v
    msg.log('Writing ', floorc_json)
    with open(G.FLOORC_JSON_PATH, 'w') as fd:
        fd.write(json.dumps(floorc_json, indent=4, sort_keys=True, separators=(',', ': ')))


def validate_auth(auth):
    if type(auth) != dict:
        msg.error('floorc.json validation error: Auth section is not an object!')
        return False
    to_delete = []
    for k, v in auth.items():
        if type(v) != dict:
            msg.error('floorc.json validation error: host "', k, '" has invalid auth credentials. Did you put a setting in the auth section?')
            to_delete.append(k)
            break
        for key in ['username', 'api_key', 'secret']:
            if not v.get(key):
                msg.error('floorc.json validation error: host "', k, '" missing "', key, '"')
                to_delete.append(k)
                break
    for k in to_delete:
        del auth[k]
    return len(to_delete) == 0


def can_auth(host=None):
    if host is None:
        host = len(G.AUTH) and list(G.AUTH.keys())[0] or G.DEFAULT_HOST
    auth = G.AUTH.get(host)
    if type(auth) == dict:
        return bool((auth.get('username') or auth.get('api_key')) and auth.get('secret'))
    return False


cancelled_timeouts = set()
timeout_ids = set()


def set_timeout(func, timeout, *args, **kwargs):
    return _set_timeout(func, timeout, False, *args, **kwargs)


def set_interval(func, timeout, *args, **kwargs):
    return _set_timeout(func, timeout, True, *args, **kwargs)


def _set_timeout(func, timeout, repeat, *args, **kwargs):
    timeout_id = set_timeout._top_timeout_id
    if timeout_id > 100000:
        set_timeout._top_timeout_id = 0
    else:
        set_timeout._top_timeout_id += 1

    try:
        from . import api
    except ImportError:
        import api

    @api.send_errors
    def timeout_func():
        timeout_ids.discard(timeout_id)
        if timeout_id in cancelled_timeouts:
            cancelled_timeouts.remove(timeout_id)
            return

        func(*args, **kwargs)

        if repeat:
            editor.set_timeout(timeout_func, timeout)
            timeout_ids.add(timeout_id)

    editor.set_timeout(timeout_func, timeout)
    timeout_ids.add(timeout_id)
    return timeout_id

set_timeout._top_timeout_id = 0


def cancel_timeout(timeout_id):
    if timeout_id in timeout_ids:
        cancelled_timeouts.add(timeout_id)


rate_limits = {}


def rate_limit(name, timeout, func, *args, **kwargs):
    if rate_limits.get(name):
        return
    rate_limits[name] = time.time()
    func(*args, **kwargs)

    def delete_limit():
        del rate_limits[name]

    set_timeout(delete_limit, timeout, *args, **kwargs)


def parse_url(workspace_url):
    secure = G.SECURE
    owner = None
    workspace_name = None
    # owner/workspacename
    result = re.match('^([-\@\+\.\w]+)/([-\.\w]+)$', workspace_url)
    if result:
        workspace_url = 'https://' + G.DEFAULT_HOST + '/' + workspace_url
    parsed_url = urlparse(workspace_url)
    port = parsed_url.port
    if G.DEBUG and parsed_url.scheme == 'http':
        # Only obey http if we're debugging
        if not port:
            port = 3148
        secure = False

    if not port:
        port = G.DEFAULT_PORT

    # Allow /file/...
    result = re.match('^/([-\@\+\.\w]+)/([-\.\w]+)/?.*$', parsed_url.path)
    if not result:
        # Old style URL. Do not remove. People still have these in their persistent.json
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
    workspace_url = '%s://%s%s/%s/%s' % (proto, host, port, r['owner'], r['workspace'])
    p = r.get('path')
    if p:
        workspace_url += '/file/%s' % p
        line = r.get('line')
        if line:
            workspace_url += ':%s' % line
    return workspace_url


def normalize_url(workspace_url):
    return to_workspace_url(parse_url(workspace_url))


def get_full_path(p):
    full_path = os.path.join(G.PROJECT_PATH, p)
    return unfuck_path(full_path)


def unfuck_path(p):
    return os.path.normpath(p)


def to_rel_path(p):
    return os.path.relpath(p, G.PROJECT_PATH).replace(os.sep, '/')


def to_scheme(secure):
    if secure is True:
        return 'https'
    return 'http'


def is_shared(p):
    if not G.AGENT or not G.AGENT.joined_workspace:
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

    try:
        with open(path, 'w') as floo_fd:
            floo_fd.write(json.dumps(floo_json, indent=4, sort_keys=True, separators=(',', ': ')))
    except Exception as e:
        msg.warn('Couldn\'t update .floo file: ', floo_json, ': ', str_e(e))


def read_floo_file(path):
    floo_file = os.path.join(path, '.floo')

    info = {}
    try:
        floo_info = open(floo_file, 'rb').read().decode('utf-8')
        info = json.loads(floo_info)
    except (IOError, OSError):
        pass
    except Exception as e:
        msg.warn('Couldn\'t read .floo file: ', floo_file, ': ', str_e(e))
    return info


def get_persistent_data(per_path=None):
    per_data = {'recent_workspaces': [], 'workspaces': {}}
    per_path = per_path or os.path.join(G.BASE_DIR, 'persistent.json')
    try:
        per = open(per_path, 'rb')
    except (IOError, OSError):
        msg.debug('Failed to open ', per_path, '. Recent workspace list will be empty.')
        return per_data
    try:
        data = per.read().decode('utf-8')
        persistent_data = json.loads(data)
    except Exception as e:
        msg.debug('Failed to parse ', per_path, '. Recent workspace list will be empty.')
        msg.debug(str_e(e))
        msg.debug(data)
        return per_data
    if 'recent_workspaces' not in persistent_data:
        persistent_data['recent_workspaces'] = []
    if 'workspaces' not in persistent_data:
        persistent_data['workspaces'] = {}
    return persistent_data


def update_persistent_data(data):
    seen = set()
    recent_workspaces = []
    for x in data['recent_workspaces']:
        try:
            if x['url'] in seen:
                continue
            seen.add(x['url'])
            recent_workspaces.append(x)
        except Exception as e:
            msg.debug(str_e(e))

    data['recent_workspaces'] = recent_workspaces
    per_path = os.path.join(G.BASE_DIR, 'persistent.json')
    with open(per_path, 'wb') as per:
        per.write(json.dumps(data, indent=2).encode('utf-8'))


# Cleans up URLs in persistent.json
def normalize_persistent_data():
    persistent_data = get_persistent_data()
    for rw in persistent_data['recent_workspaces']:
        rw['url'] = normalize_url(rw['url'])

    for owner, workspaces in persistent_data['workspaces'].items():
        for name, workspace in workspaces.items():
            workspace['url'] = normalize_url(workspace['url'])
            workspace['path'] = unfuck_path(workspace['path'])
    update_persistent_data(persistent_data)


def add_workspace_to_persistent_json(owner, name, url, path):
    d = get_persistent_data()
    workspaces = d['workspaces']
    if owner not in workspaces:
        workspaces[owner] = {}
    workspaces[owner][name] = {'url': url, 'path': path}
    update_persistent_data(d)


def update_recent_workspaces(workspace_url):
    d = get_persistent_data()
    recent_workspaces = d.get('recent_workspaces', [])
    recent_workspaces.insert(0, {'url': workspace_url})
    recent_workspaces = recent_workspaces[:100]
    seen = set()
    new = []
    for r in recent_workspaces:
        string = json.dumps(r)
        if string not in seen:
            new.append(r)
            seen.add(string)
    d['recent_workspaces'] = new
    update_persistent_data(d)


def get_workspace_by_path(path, _filter):
    path = unfuck_path(path)
    for owner, workspaces in get_persistent_data()['workspaces'].items():
        for name, workspace in workspaces.items():
            if unfuck_path(workspace['path']) == path:
                r = _filter(workspace['url'])
                if r:
                    return r


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
        if e.errno != errno.EEXIST:
            editor.error_message('Cannot create directory {0}.\n{1}'.format(path, str_e(e)))
            raise


def get_line_endings(path):
    try:
        with open(path, 'rb') as fd:
            line = fd.readline()
    except Exception:
        return
    if not line:
        return
    chunk = line[-2:]
    if chunk == "\r\n":
        return "\r\n"
    if chunk[-1:] == "\n":
        return "\n"


def save_buf(buf):
    path = get_full_path(buf['path'])
    mkdir(os.path.split(path)[0])
    if buf['encoding'] == 'utf8':
        newline = get_line_endings(path) or editor.get_line_endings(path)
    try:
        with open(path, 'wb') as fd:
            if buf['encoding'] == 'utf8':
                out = buf['buf']
                if newline != '\n':
                    out = out.split('\n')
                    out = newline.join(out)
                fd.write(out.encode('utf-8'))
            else:
                fd.write(buf['buf'])
    except Exception as e:
        msg.error('Error saving buf: ', str_e(e))


def _unwind_generator(gen_expr, cb=None, res=None):
    try:
        while True:
            maybe_func = res
            args = []
            # if the first arg is callable, we need to call it (and assume the last argument is a callback)
            if type(res) == tuple:
                maybe_func = len(res) and res[0]

            if not callable(maybe_func):
                # send only accepts one argument... this is slightly dangerous if
                # we ever just return a tuple of one elemetn
                # TODO: catch no generator
                if type(res) == tuple and len(res) == 1:
                    res = gen_expr.send(res[0])
                else:
                    res = gen_expr.send(res)
                continue

            def f(*args):
                return _unwind_generator(gen_expr, cb, args)

            try:
                args = list(res)[1:]
            except:
                # assume not iterable
                args = []

            args.append(f)
            return maybe_func(*args)

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
        "Opera",
        "windows-default",
    ]
    for browser in valid_browsers:
        try:
            webbrowser.get(browser)
            return True
        except Exception:
            continue
    return False
