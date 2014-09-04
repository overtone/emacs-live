import os
import json
import errno
from collections import defaultdict

try:
    from . import shared as G
    from . import utils
except (ImportError, ValueError):
    import shared as G
    import utils


def rename_floobits_dir():
    # TODO: one day this can be removed (once all our users have updated)
    old_colab_dir = os.path.realpath(os.path.expanduser(os.path.join('~', '.floobits')))
    if os.path.isdir(old_colab_dir) and not os.path.exists(G.BASE_DIR):
        print('renaming %s to %s' % (old_colab_dir, G.BASE_DIR))
        os.rename(old_colab_dir, G.BASE_DIR)
        os.symlink(G.BASE_DIR, old_colab_dir)


def get_legacy_projects():
    a = ['msgs.floobits.log', 'persistent.json']
    owners = os.listdir(G.COLAB_DIR)
    floorc_json = defaultdict(defaultdict)
    for owner in owners:
        if len(owner) > 0 and owner[0] == '.':
            continue
        if owner in a:
            continue
        workspaces_path = os.path.join(G.COLAB_DIR, owner)
        try:
            workspaces = os.listdir(workspaces_path)
        except OSError:
            continue
        for workspace in workspaces:
            workspace_path = os.path.join(workspaces_path, workspace)
            workspace_path = os.path.realpath(workspace_path)
            try:
                fd = open(os.path.join(workspace_path, '.floo'), 'rb')
                url = json.loads(fd.read())['url']
                fd.close()
            except Exception:
                url = utils.to_workspace_url({
                    'port': 3448, 'secure': True, 'host': 'floobits.com', 'owner': owner, 'workspace': workspace
                })
            floorc_json[owner][workspace] = {
                'path': workspace_path,
                'url': url
            }

    return floorc_json


def migrate_symlinks():
    data = {}
    old_path = os.path.join(G.COLAB_DIR, 'persistent.json')
    if not os.path.exists(old_path):
        return
    old_data = utils.get_persistent_data(old_path)
    data['workspaces'] = get_legacy_projects()
    data['recent_workspaces'] = old_data.get('recent_workspaces')
    utils.update_persistent_data(data)
    try:
        os.unlink(old_path)
        os.unlink(os.path.join(G.COLAB_DIR, 'msgs.floobits.log'))
    except Exception:
        pass


def __load_floorc():
    """try to read settings out of the .floorc file"""
    s = {}
    try:
        fd = open(G.FLOORC_PATH, 'r')
    except IOError as e:
        if e.errno == errno.ENOENT:
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


def migrate_floorc():
    s = __load_floorc()
    default_host = s.get('DEFAULT_HOST', G.DEFAULT_HOST)

    floorc_json = {
        'auth': {
            default_host: {}
        }
    }
    for k, v in s.items():
        k = k.lower()
        try:
            v = int(v)
        except Exception:
            pass

        if k in ['username', 'secret', 'api_key']:
            floorc_json['auth'][default_host][k] = v
        else:
            floorc_json[k] = v
    with open(G.FLOORC_JSON_PATH, 'w') as fd:
        fd.write(json.dumps(floorc_json, indent=4, sort_keys=True))
