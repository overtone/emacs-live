import os
import json
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
