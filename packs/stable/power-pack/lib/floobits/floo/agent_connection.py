import base64
import webbrowser

from floo.common.handlers import floo_handler
from floo.common import msg, utils, shared as G


class AgentConnection(floo_handler.FlooHandler):

    def __init__(self, owner, workspace, emacs_handler, auth, join_action):
        super(AgentConnection, self).__init__(owner, workspace, auth, join_action)
        self.emacs_handler = emacs_handler

    def get_view_text_by_path(self, rel_path):
        return self.emacs_handler.get_view_text_by_path(rel_path)

    def stop(self):
        super(AgentConnection, self).stop()
        self.emacs_handler.stop()

    def get_view(self, buf_id):
        return self.emacs_handler.get_view(buf_id)

    def ok_cancel_dialog(self, prompt, cb):
        return self.emacs_handler.ui.user_y_or_n(self.emacs_handler, prompt, '', cb)

    def to_emacs(self, name, data):
        data['name'] = name
        self.emacs_handler.send(data)

    def stomp_prompt(self, changed_bufs, missing_bufs, new_files, ignored, cb):

        def pluralize(arg):
            return arg != 1 and 's' or ''

        overwrite_local = ''
        overwrite_remote = ''
        missing = [buf['path'] for buf in missing_bufs]
        changed = [buf['path'] for buf in changed_bufs]

        to_remove = set(missing + ignored)
        to_upload = set(new_files + changed).difference(to_remove)
        to_fetch = changed + missing
        to_upload_len = len(to_upload)
        to_remove_len = len(to_remove)
        remote_len = to_remove_len + to_upload_len
        to_fetch_len = len(to_fetch)

        msg.log('To fetch: ', ', '.join(to_fetch))
        msg.log('To upload: ', ', '.join(to_upload))
        msg.log('To remove: ', ', '.join(to_remove))

        if not to_fetch:
            overwrite_local = 'Fetch nothing'
        elif to_fetch_len < 5:
            overwrite_local = 'Fetch %s' % ', '.join(to_fetch)
        else:
            overwrite_local = 'Fetch %s file%s' % (to_fetch_len, pluralize(to_fetch_len))

        if to_upload_len < 5:
            to_upload_str = 'upload %s' % ', '.join(to_upload)
        else:
            to_upload_str = 'upload %s' % to_upload_len

        if to_remove_len < 5:
            to_remove_str = 'remove %s' % ', '.join(to_remove)
        else:
            to_remove_str = 'remove %s' % to_remove_len

        if to_upload:
            overwrite_remote += to_upload_str
            if to_remove:
                overwrite_remote += ' and '
        if to_remove:
            overwrite_remote += to_remove_str

        if remote_len >= 5 and overwrite_remote:
            overwrite_remote += ' files'

        overwrite_remote = overwrite_remote.capitalize()

        action = 'Overwrite'
        # TODO: change action based on numbers of stuff
        choices = [
            '%s %s remote file%s (%s).' % (action, remote_len, pluralize(remote_len), overwrite_remote),
            '%s %s local file%s (%s).' % (action, to_fetch_len, pluralize(to_fetch_len), overwrite_local),
            'Cancel',
        ]

        prompt = 'Your copy of %s/%s is out of sync. Do you want to:' % (self.owner, self.workspace)

        self.emacs_handler.ui.user_select(self.emacs_handler, prompt, choices, None, lambda c, i: cb(i))

    @utils.inlined_callbacks
    def prompt_join_hangout(self, hangout_url):
        join = yield self.ok_cancel_dialog, 'This workspace is being edited in a hangout. Would you like to join the hangout?'
        if not join:
            return
        try:
            webbrowser.open(hangout_url, new=2, autoraise=True)
        except Exception as e:
            msg.error("Couldn't open a browser: %s" % (str(e)))

    def _on_room_info(self, data):
        def send_room_info():
            self.to_emacs('room_info', {
                'perms': data['perms'],
                'project_path': G.PROJECT_PATH,
                'workspace_name': data['room_name']
            })
        self.once('room_info', send_room_info)
        super(AgentConnection, self)._on_room_info(data)

    def _on_create_buf(self, data):
        if data['encoding'] == 'base64':
            data['buf'] = base64.b64decode(data['buf'])
        self.bufs[data['id']] = data
        self.paths_to_ids[data['path']] = data['id']
        abs_path = utils.get_full_path(data['path'])

        self.to_emacs('create_buf', {
            'full_path': utils.get_full_path(data['path']),
            'path': data['path'],
            'username': data.get('username', ''),
        })

        if abs_path not in self.emacs_handler.emacs_bufs:
            utils.save_buf(data)
            return
        text = self.emacs_handler.emacs_bufs.get(abs_path)[0]
        if text == data['buf']:
            return
        self.emacs_handler.bufs_changed.append(data['id'])

    def _on_delete_buf(self, data):
        buf_id = int(data['id'])
        buf = self.bufs[buf_id]
        path = buf['path']
        try:
            super(AgentConnection, self)._on_delete_buf(data)
        except Exception as e:
            msg.debug('Unable to delete buf %s: %s' % (path, str(e)))
        else:
            self.to_emacs('delete_buf', {
                'full_path': utils.get_full_path(path),
                'path': path,
                'username': data.get('username', ''),
            })

    def _on_rename_buf(self, data):
        # This can screw up if someone else renames the buffer around the same time as us. Oh well.
        msg.debug('asdf %s' % data)
        buf = self.get_buf_by_path(utils.get_full_path(data['old_path']))
        if buf:
            return super(AgentConnection, self)._on_rename_buf(data)
        msg.debug('We already renamed %s. Skipping' % data['old_path'])

    def highlight(self, user=None, **kwargs):
        # Emacs stores highlight state separately, outside of python
        if user is not None:
            self.to_emacs('follow_user', {'username': user})
        msg.log("Sent %s to emacs follow_user" % user)

    def _on_highlight(self, data):
        buf = self.bufs[data['id']]
        # TODO: save highlights for when user opens the buffer in emacs
        self.to_emacs('highlight', {
            'full_path': utils.get_full_path(buf['path']),
            'ranges': data['ranges'],
            'user_id': data['user_id'],
            'username': data.get('username', 'unknown user'),
            'following': data.get('following', False),
            'ping': data.get('ping', False)
        })

    def _on_msg(self, data):
        msg.log('msg')
