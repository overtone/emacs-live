# coding: utf-8
import sys
import hashlib
from collections import defaultdict

try:
    from . import editor, emui
    from .common import msg, shared as G, utils
    from .view import View
    # from .common.exc_fmt import str_e
    from .common.handlers import base
    from .emacs_protocol import EmacsProtocol
except (ImportError, ValueError):
    import editor
    import emui
    from common import msg, shared as G, utils
    from view import View
    # from common.exc_fmt import str_e
    from common.handlers import base
    from emacs_protocol import EmacsProtocol


try:
    import urllib
    from urllib import request
    Request = request.Request
    urlopen = request.urlopen
    HTTPError = urllib.error.HTTPError
    URLError = urllib.error.URLError
    assert Request and urlopen and HTTPError and URLError
except ImportError:
    import urllib2
    Request = urllib2.Request
    urlopen = urllib2.urlopen
    HTTPError = urllib2.HTTPError
    URLError = urllib2.URLError


def has_perm(perm):
    def outer(f):
        def inner(*args, **kwargs):
            if perm in G.PERMS:
                return f(*args, **kwargs)
        return inner
    return outer


class EmacsHandler(base.BaseHandler):
    PROTOCOL = EmacsProtocol

    def __init__(self, *args, **kwargs):
        super(EmacsHandler, self).__init__(*args, **kwargs)
        self.agent = None  # agent handler (to the backend connection)
        self.views = {}
        self.emacs_bufs = defaultdict(lambda: [""])
        self.bufs_changed = []
        self.ui = emui.Emui()

        def set_agent(a):
            self.agent = a
        self.ui.on("agent", set_agent)

    def stop(self):
        sys.exit()

    def get_view_text_by_path(self, rel_path):
        full_path = utils.get_full_path(rel_path)
        emacs_buf = self.emacs_bufs.get(full_path)
        if emacs_buf:
            return emacs_buf[0]

    def error_message(self, msg):
        print(msg)
        self.send({'name': 'error', 'msg': msg})

    def status_message(self, *args, **kwargs):
        print(args, kwargs)

    def send_to_floobits(self, data):
        self.agent.send(data)

    def get_buf_by_path(self, path):
        if not self.agent:
            return None
        return self.agent.get_buf_by_path(path)

    def tick(self):
        reported = set()
        while self.bufs_changed:
            buf_id = self.bufs_changed.pop()
            view = self.get_view(buf_id)
            if view is None:
                msg.debug('View for buf %s no longer exists. Ignoring change event' % buf_id)
                continue
            buf = view.buf
            if view.is_loading():
                msg.debug('View for buf %s is not ready. Ignoring change event' % buf['id'])
                continue
            if 'patch' not in G.PERMS:
                continue
            vb_id = view.native_id
            if vb_id in reported:
                continue
            if 'buf' not in buf:
                msg.debug('No data for buf %s %s yet. Skipping sending patch' % (buf['id'], buf['path']))
                continue

            reported.add(vb_id)
            patch = utils.FlooPatch(view.get_text(), view.buf)
            # Update the current copy of the buffer
            buf['buf'] = patch.current
            buf['md5'] = hashlib.md5(patch.current.encode('utf-8')).hexdigest()
            self.send_to_floobits(patch.to_json())

    def on_connect(self):
        msg.log("have an emacs!")

    def create_view(self, buf, emacs_buf=None):
        v = View(self, buf, emacs_buf)
        self.views[buf['id']] = v
        return v

    def get_view(self, buf_id):
        """Warning: side effects!"""
        # return self.agent.get_view(buf_id)
        view = self.views.get(buf_id)
        if view:
            return view
        buf = self.agent.bufs[buf_id]
        full_path = utils.get_full_path(buf['path'])
        emacs_buf = self.emacs_bufs.get(full_path)
        if emacs_buf:
            view = self.create_view(buf, emacs_buf)
        return view

    def get_view_by_path(self, path):
        """Warning: side effects!"""
        if not path:
            return None
        buf = self.get_buf_by_path(path)
        if not buf:
            msg.debug("buf not found for path %s" % path)
            return None
        view = self.get_view(buf['id'])
        if not view:
            msg.debug("view not found for %s %s" % (buf['id'], buf['path']))
            return None
        return view

    def update_view(self, data, view):
        view.set_text(data['buf'])

    def _on_user_input(self, data):
        cb_id = int(data['id'])
        self.ui.on_user_input(cb_id, data.get("response"))

    def _on_set_follow_mode(self, req):
        G.FOLLOW_USERS = set()
        msg.log('follow mode is %s' % ((req.get('follow_mode') and 'enabled') or 'disabled'))

    def _on_change(self, req):
        path = req['full_path']
        view = self.get_view_by_path(path)
        changed = req['changed']
        begin = req['begin']
        old_length = req['old_length']
        self.emacs_bufs[path][0] = "%s%s%s" % (self.emacs_bufs[path][0][:begin - 1], changed, self.emacs_bufs[path][0][begin - 1 + old_length:])
        if not view:
            return
        self.bufs_changed.append(view.buf['id'])

    @has_perm('highlight')
    def _on_highlight(self, req):
        view = self.get_view_by_path(req['full_path'])
        if not view:
            return
        highlight_json = {
            'id': view.buf['id'],
            'name': 'highlight',
            'ranges': req['ranges'],
            'following': bool(req['following']),
            'ping': req.get("ping"),
        }
        msg.debug("sending highlight upstream %s" % highlight_json)
        self.send_to_floobits(highlight_json)

    @has_perm('create_buf')
    def _on_create_buf(self, req):
        # TODO: use the view state if it exists instead of uploading the on-disk state
        self.agent.upload(req['full_path'])

    @has_perm('delete_buf')
    def _on_delete_buf(self, req):
        buf = self.get_buf_by_path(req['path'])
        if not buf:
            msg.debug('No buffer for path %s' % req['path'])
            return
        msg.log('deleting buffer ', buf['path'])
        self.send_to_floobits({
            'name': 'delete_buf',
            'id': buf['id'],
        })

    @has_perm('rename_buf')
    def _on_rename_buf(self, req):
        old_path = utils.to_rel_path(req['old_path'])
        buf = self.get_buf_by_path(old_path)
        if not buf:
            msg.debug('No buffer for path %s' % req['path'])
            return
        path = utils.to_rel_path(req['path'])
        if not utils.is_shared(path):
            msg.log('New path %s is not shared. Discarding rename event.' % path)
            return
        buf_id = buf['id']
        self.send_to_floobits({
            'name': 'rename_buf',
            'id': buf['id'],
            'path': path,
        })
        # KANS: is this right? old code...
        old_path = self.agent.bufs[buf_id]['path']
        del self.agent.paths_to_ids[old_path]
        self.agent.paths_to_ids[path] = buf_id
        self.agent.bufs[buf_id]['path'] = path

    @has_perm('saved')
    def _on_saved(self, req):
        buf = self.get_buf_by_path(req['path'])
        if not buf:
            msg.debug('No buffer for path %s' % req['path'])
            return
        self.send_to_floobits({
            'name': 'saved',
            'id': buf['id'],
        })

    @has_perm('patch')
    def _on_revert(self, req):
        path = req['full_path']
        view = self.get_view_by_path(path)
        self.emacs_bufs[path][0] = req['buf']
        if not view:
            if 'create_buf' in G.PERMS and utils.is_shared(path) and G.IGNORE and not G.IGNORE.is_ignored(path):
                self.agent._upload(path, text=req['buf'])
            return
        self.bufs_changed.append(view.buf['id'])

    def _on_buffer_list_change(self, req):
        added = req.get('added') or {}
        for path, text in added.items():
            buf = self.get_buf_by_path(path)
            buf_id = buf and int(buf.get('id'))
            d = buf and 'buf' in buf and self.agent.on_load.get(buf_id)
            if d:
                self.emacs_bufs[path][0] = buf['buf']
            else:
                self.emacs_bufs[path][0] = text
            if not buf:
                msg.debug('no buf for path %s' % path)
                if 'create_buf' in G.PERMS and utils.is_shared(path) and G.IGNORE and not G.IGNORE.is_ignored(path):
                    self.agent._upload(path, text=text)
                elif path in self.emacs_bufs:
                    del self.emacs_bufs[path]
                continue
            view = self.views.get(buf_id)
            if view is None:
                self.get_view(buf_id)
            elif view.is_loading():
                view._emacs_buf = self.emacs_bufs[path]
            else:
                msg.debug('view for buf %s already exists. this is not good. we got out of sync' % buf['path'])
            if d:
                del self.agent.on_load[buf_id]
                for _, f in d.items():
                    f()

        deleted = req.get('deleted') or []
        for path in deleted:
            if self.emacs_bufs.get(path) is None:
                msg.debug('emacs deleted %s but we already deleted it from emacs_bufs' % path)
            if path in self.emacs_bufs:
                del self.emacs_bufs[path]
            buf = self.get_buf_by_path(path)
            if buf and buf['id'] in self.views:
                del self.views[buf['id']]

        seen = set()
        current = req.get('current') or []
        for path in current:
            if self.emacs_bufs.get(path) is None:
                msg.debug('We should have buffer %s in emacs_bufs but we don\'t' % path)
            else:
                seen.add(path)

        for buf_id, view in self.views.items():
            if utils.get_full_path(view.buf['path']) not in seen:
                msg.debug('We should not have buffer %s in our views but we do.' % view.buf['path'])

    def _on_open_workspace(self, req):
        self.ui.open_workspace()

    def _on_open_workspace_settings(self, req):
        self.ui.open_workspace_settings()

    def _on_share_dir(self, data):
        editor.line_endings = data['line_endings'].find("unix") >= 0 and "\n" or "\r\n"
        self.ui.share_dir(self, data['dir_to_share'], {'perms': data['perms']})

    def _on_join_workspace(self, data):
        workspace = data['workspace']
        owner = data['workspace_owner']
        host = data['host']
        current_directory = data['current_directory']
        editor.line_endings = data['line_endings'].find("unix") >= 0 and "\n" or "\r\n"
        self.ui.join_workspace(self, host, workspace, owner, [current_directory])

    def _on_setting(self, data):
        setattr(G, data['name'], data['value'])
        if data['name'] == 'debug':
            utils.update_log_level()

    def _on_pinocchio(self, data):
        self.ui.pinocchio()

    def _on_follow_user(self, data):
        self.ui.follow_user(self)

    def _on_delete_workspace(self, data):
        self.ui.delete_workspace(self, lambda: sys.exit(0))
