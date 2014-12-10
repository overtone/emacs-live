import sys
import base64
import json
import subprocess
import traceback
from functools import wraps

try:
    import ssl
except ImportError:
    ssl = False

PY2 = sys.version_info < (3, 0)


try:
    import __builtin__
    str_instances = (str, __builtin__.basestring)
except Exception:
    str_instances = (str, )

try:
    import urllib
    from urllib.request import Request, urlopen
    HTTPError = urllib.error.HTTPError
    URLError = urllib.error.URLError
except (AttributeError, ImportError, ValueError):
    import urllib2
    from urllib2 import Request, urlopen
    HTTPError = urllib2.HTTPError
    URLError = urllib2.URLError

try:
    from .. import editor
    from . import msg, shared as G, utils
except ImportError:
    import editor
    import msg
    import shared as G
    import utils


def get_basic_auth(host):
    username = G.AUTH.get(host, {}).get('username')
    secret = G.AUTH.get(host, {}).get('secret')
    if username is None or secret is None:
        return
    basic_auth = ('%s:%s' % (username, secret)).encode('utf-8')
    basic_auth = base64.encodestring(basic_auth)
    return basic_auth.decode('ascii').replace('\n', '')


class APIResponse():
    def __init__(self, r):
        self.body = None
        if isinstance(r, bytes):
            r = r.decode('utf-8')
        if isinstance(r, str_instances):
            lines = r.split('\n')
            self.code = int(lines[0])
            if self.code != 204:
                self.body = json.loads('\n'.join(lines[1:]))
        elif isinstance(r, URLError):
            # horrible hack, but lots of other stuff checks the response code :/
            self.code = 500
            self.body = r
        else:
            # Hopefully this is an HTTPError
            self.code = r.code
            if self.code != 204:
                self.body = json.loads(r.read().decode("utf-8"))


def proxy_api_request(host, url, data, method):
    args = ['python', '-m', 'floo.proxy', '--host', host, '--url', url]
    if data:
        args += ["--data", json.dumps(data)]
    if method:
        args += ["--method", method]
    msg.log('Running ', ' '.join(args), ' (', G.PLUGIN_PATH, ')')
    proc = subprocess.Popen(args, cwd=G.PLUGIN_PATH, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    (stdout, stderr) = proc.communicate()
    if stderr:
        raise IOError(stderr)

    if proc.poll() != 0:
        raise IOError(stdout)
    r = APIResponse(stdout)
    return r


def user_agent():
    return 'Floobits Plugin %s %s %s py-%s.%s' % (
        editor.name(),
        G.__PLUGIN_VERSION__,
        editor.platform(),
        sys.version_info[0],
        sys.version_info[1]
    )


def hit_url(host, url, data, method):
    if data:
        data = json.dumps(data).encode('utf-8')
    r = Request(url, data=data)
    r.method = method
    r.get_method = lambda: method
    auth = get_basic_auth(host)
    if auth:
        r.add_header('Authorization', 'Basic %s' % auth)
    r.add_header('Accept', 'application/json')
    r.add_header('Content-type', 'application/json')
    r.add_header('User-Agent', user_agent())
    return urlopen(r, timeout=5)


def api_request(host, url, data=None, method=None):
    if data:
        method = method or 'POST'
    else:
        method = method or 'GET'
    if ssl is False:
        return proxy_api_request(host, url, data, method)
    try:
        r = hit_url(host, url, data, method)
    except HTTPError as e:
        r = e
    except URLError as e:
        msg.warn('Error hitting url ', url, ': ', e)
        r = e
        if not PY2:
            msg.warn('Retrying using system python...')
            return proxy_api_request(host, url, data, method)
    return APIResponse(r)


def create_workspace(host, post_data):
    api_url = 'https://%s/api/workspace' % host
    return api_request(host, api_url, post_data)


def delete_workspace(host, owner, workspace):
    api_url = 'https://%s/api/workspace/%s/%s' % (host, owner, workspace)
    return api_request(host, api_url, method='DELETE')


def update_workspace(workspace_url, data):
    result = utils.parse_url(workspace_url)
    api_url = 'https://%s/api/workspace/%s/%s' % (result['host'], result['owner'], result['workspace'])
    return api_request(result['host'], api_url, data, method='PUT')


def get_workspace_by_url(url):
    result = utils.parse_url(url)
    api_url = 'https://%s/api/workspace/%s/%s' % (result['host'], result['owner'], result['workspace'])
    return api_request(result['host'], api_url)


def get_workspace(host, owner, workspace):
    api_url = 'https://%s/api/workspace/%s/%s' % (host, owner, workspace)
    return api_request(host, api_url)


def get_workspaces(host):
    api_url = 'https://%s/api/workspaces/can/view' % (host)
    return api_request(host, api_url)


def get_orgs(host):
    api_url = 'https://%s/api/orgs' % (host)
    return api_request(host, api_url)


def get_orgs_can_admin(host):
    api_url = 'https://%s/api/orgs/can/admin' % (host)
    return api_request(host, api_url)


def send_error(description=None, exception=None):
    G.ERROR_COUNT += 1
    if G.ERRORS_SENT >= G.MAX_ERROR_REPORTS:
        msg.warn('Already sent ', G.ERRORS_SENT, ' errors this session. Not sending any more.\n', description, exception)
        return
    data = {
        'jsondump': {
            'error_count': G.ERROR_COUNT
        },
        'message': {},
        'dir': G.COLAB_DIR,
    }
    if G.AGENT:
        data['owner'] = getattr(G.AGENT, "owner", None)
        data['username'] = getattr(G.AGENT, "username", None)
        data['workspace'] = getattr(G.AGENT, "workspace", None)
    if exception:
        data['message'] = {
            'description': str(exception),
            'stack': traceback.format_exc(exception)
        }
    msg.log('Floobits plugin error! Sending exception report: ', data['message'])
    if description:
        data['message']['description'] = description
    try:
        # TODO: use G.AGENT.proto.host?
        api_url = 'https://%s/api/log' % (G.DEFAULT_HOST)
        r = api_request(G.DEFAULT_HOST, api_url, data)
        G.ERRORS_SENT += 1
        return r
    except Exception as e:
        print(e)


def send_errors(f):
    @wraps(f)
    def wrapped(*args, **kwargs):
        try:
            return f(*args, **kwargs)
        except Exception as e:
            send_error(None, e)
            raise
    return wrapped
