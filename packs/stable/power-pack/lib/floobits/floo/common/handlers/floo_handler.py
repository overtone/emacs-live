import os
import sys
import hashlib
import base64
from operator import attrgetter

try:
    from . import base
    from ..reactor import reactor
    from ..lib import DMP
    from .. import msg, ignore, shared as G, utils
    from ... import editor
    from ..protocols import floo_proto
except (ImportError, ValueError) as e:
    import base
    from floo import editor
    from floo.common.lib import DMP
    from floo.common.reactor import reactor
    from floo.common import msg, ignore, shared as G, utils
    from floo.common.protocols import floo_proto

try:
    unicode()
except NameError:
    unicode = str

MAX_WORKSPACE_SIZE = 50000000  # 50MB


class FlooHandler(base.BaseHandler):
    PROTOCOL = floo_proto.FlooProtocol

    def __init__(self, owner, workspace, get_bufs=True):
        super(FlooHandler, self).__init__()
        self.owner = owner
        self.workspace = workspace
        self.should_get_bufs = get_bufs
        self.reset()
        self.bufs = {}
        self.paths_to_ids = {}

    def _on_highlight(self, data):
        raise NotImplementedError("_on_highlight not implemented")

    def ok_cancel_dialog(self, msg, cb=None):
        raise NotImplementedError("ok_cancel_dialog not implemented.")

    def get_view(self, buf_id):
        raise NotImplementedError("get_view not implemented")

    def get_username_by_id(self, user_id):
        try:
            return self.workspace_info['users'][str(user_id)]['username']
        except Exception:
            return ""

    def get_buf_by_path(self, path):
        try:
            p = utils.to_rel_path(path)
        except ValueError:
            return
        buf_id = self.paths_to_ids.get(p)
        if buf_id:
            return self.bufs.get(buf_id)

    def get_buf(self, buf_id, view=None):
        self.send({
            'name': 'get_buf',
            'id': buf_id
        })
        buf = self.bufs[buf_id]
        msg.warn('Syncing buffer %s for consistency.' % buf['path'])
        if 'buf' in buf:
            del buf['buf']

        if view:
            view.set_read_only(True)
            view.set_status('Floobits locked this file until it is synced.')

    def save_view(self, view):
        view.save()

    def on_connect(self):
        self.reload_settings()

        req = {
            'username': self.username,
            'secret': self.secret,
            'room': self.workspace,
            'room_owner': self.owner,
            'client': self.client,
            'platform': sys.platform,
            'supported_encodings': ['utf8', 'base64'],
            'version': G.__VERSION__
        }

        if self.api_key:
            req['api_key'] = self.api_key
        self.send(req)

    @property
    def workspace_url(self):
        protocol = self.proto.secure and 'https' or 'http'
        return '{protocol}://{host}/{owner}/{name}'.format(protocol=protocol, host=self.proto.host, owner=self.owner, name=self.workspace)

    def reset(self):
        self.bufs = {}
        self.paths_to_ids = {}
        self.save_on_get_bufs = set()

    def _on_patch(self, data):
        buf_id = data['id']
        buf = self.bufs[buf_id]
        if 'buf' not in buf:
            msg.debug('buf %s not populated yet. not patching' % buf['path'])
            return

        if buf['encoding'] == 'base64':
            # TODO apply binary patches
            return self.get_buf(buf_id, None)

        if len(data['patch']) == 0:
            msg.debug('wtf? no patches to apply. server is being stupid')
            return

        msg.debug('patch is', data['patch'])
        dmp_patches = DMP.patch_fromText(data['patch'])
        # TODO: run this in a separate thread
        old_text = buf['buf']

        view = self.get_view(buf_id)
        if view and not view.is_loading():
            view_text = view.get_text()
            if old_text == view_text:
                buf['forced_patch'] = False
            elif not buf.get('forced_patch'):
                patch = utils.FlooPatch(view_text, buf)
                # Update the current copy of the buffer
                buf['buf'] = patch.current
                buf['md5'] = hashlib.md5(patch.current.encode('utf-8')).hexdigest()
                buf['forced_patch'] = True
                msg.debug('forcing patch for %s' % buf['path'])
                self.send(patch.to_json())
                old_text = view_text
            else:
                msg.debug('forced patch is true. not sending another patch for buf %s' % buf['path'])
        md5_before = hashlib.md5(old_text.encode('utf-8')).hexdigest()
        if md5_before != data['md5_before']:
            msg.warn('starting md5s don\'t match for %s. this is dangerous!' % buf['path'])

        t = DMP.patch_apply(dmp_patches, old_text)

        clean_patch = True
        for applied_patch in t[1]:
            if not applied_patch:
                clean_patch = False
                break

        if G.DEBUG:
            if len(t[0]) == 0:
                try:
                    msg.debug('OMG EMPTY!')
                    msg.debug('Starting data:', buf['buf'])
                    msg.debug('Patch:', data['patch'])
                except Exception as e:
                    print(e)

            if '\x01' in t[0]:
                msg.debug('FOUND CRAZY BYTE IN BUFFER')
                msg.debug('Starting data:', buf['buf'])
                msg.debug('Patch:', data['patch'])

        timeout_id = buf.get('timeout_id')
        if timeout_id:
            utils.cancel_timeout(timeout_id)
            del buf['timeout_id']

        if not clean_patch:
            msg.log('Couldn\'t patch %s cleanly.' % buf['path'])
            return self.get_buf(buf_id, view)

        cur_hash = hashlib.md5(t[0].encode('utf-8')).hexdigest()
        if cur_hash != data['md5_after']:
            buf['timeout_id'] = utils.set_timeout(self.get_buf, 2000, buf_id, view)

        buf['buf'] = t[0]
        buf['md5'] = cur_hash

        if not view:
            utils.save_buf(buf)
            return

        view.apply_patches(buf, t, data['username'])

    def _on_get_buf(self, data):
        buf_id = data['id']
        buf = self.bufs.get(buf_id)
        if not buf:
            return msg.warn('no buf found: %s.  Hopefully you didn\'t need that' % data)
        timeout_id = buf.get('timeout_id')
        if timeout_id:
            utils.cancel_timeout(timeout_id)

        if data['encoding'] == 'base64':
            data['buf'] = base64.b64decode(data['buf'])

        self.bufs[buf_id] = data

        save = False
        if buf_id in self.save_on_get_bufs:
            self.save_on_get_bufs.remove(buf_id)
            save = True

        view = self.get_view(buf_id)
        if not view:
            return utils.save_buf(data)

        view.update(data)
        if save:
            view.save()

    def _on_create_buf(self, data):
        if data['encoding'] == 'base64':
            data['buf'] = base64.b64decode(data['buf'])
        self.bufs[data['id']] = data
        self.paths_to_ids[data['path']] = data['id']
        utils.save_buf(data)

    def _on_rename_buf(self, data):
        del self.paths_to_ids[data['old_path']]
        self.paths_to_ids[data['path']] = data['id']
        new = utils.get_full_path(data['path'])
        old = utils.get_full_path(data['old_path'])
        new_dir = os.path.split(new)[0]
        if new_dir:
            utils.mkdir(new_dir)
        view = self.get_view(data['id'])
        if view:
            view.rename(new)
        else:
            os.rename(old, new)
        self.bufs[data['id']]['path'] = data['path']

    def _on_delete_buf(self, data):
        path = utils.get_full_path(data['path'])
        try:
            utils.rm(path)
        except Exception:
            pass
        user_id = data.get('user_id')
        username = self.get_username_by_id(user_id)
        msg.log('%s deleted %s' % (username, path))

    @utils.inlined_callbacks
    def _on_room_info(self, data):
        self.reset()
        G.JOINED_WORKSPACE = True
        self.workspace_info = data
        G.PERMS = data['perms']

        if 'patch' not in data['perms']:
            msg.log('No patch permission. Setting buffers to read-only')
            should_send = yield self.ok_cancel_dialog, 'You don\'t have permission to edit this workspace. All files will be read-only.\n\nDo you want to request edit permission?'
            if should_send:
                self.send({'name': 'request_perms', 'perms': ['edit_room']})

        floo_json = {
            'url': utils.to_workspace_url({
                'owner': self.owner,
                'workspace': self.workspace,
                'host': self.proto.host,
                'port': self.proto.port,
                'secure': self.proto.secure,
            })
        }
        utils.update_floo_file(os.path.join(G.PROJECT_PATH, '.floo'), floo_json)

        changed_bufs = []
        missing_bufs = []
        for buf_id, buf in data['bufs'].items():
            buf_id = int(buf_id)  # json keys must be strings
            buf_path = utils.get_full_path(buf['path'])
            new_dir = os.path.dirname(buf_path)
            utils.mkdir(new_dir)
            self.bufs[buf_id] = buf
            self.paths_to_ids[buf['path']] = buf_id

            view = self.get_view(buf_id)
            if view and not view.is_loading() and buf['encoding'] == 'utf8':
                view_text = view.get_text()
                view_md5 = hashlib.md5(view_text.encode('utf-8')).hexdigest()
                if view_md5 == buf['md5']:
                    msg.debug('md5 sum matches view. not getting buffer %s' % buf['path'])
                    buf['buf'] = view_text
                    G.VIEW_TO_HASH[view.native_id] = view_md5
                elif self.should_get_bufs:
                    changed_bufs.append(buf_id)
            else:
                try:
                    buf_fd = open(buf_path, 'rb')
                    buf_buf = buf_fd.read()
                    md5 = hashlib.md5(buf_buf).hexdigest()
                    if md5 == buf['md5']:
                        msg.debug('md5 sum matches. not getting buffer %s' % buf['path'])
                        if buf['encoding'] == 'utf8':
                            buf_buf = buf_buf.decode('utf-8')
                        buf['buf'] = buf_buf
                    elif self.should_get_bufs:
                        changed_bufs.append(buf_id)
                except Exception as e:
                    msg.debug('Error calculating md5:', e)
                    missing_bufs.append(buf_id)

        if changed_bufs and self.should_get_bufs:
            if len(changed_bufs) > 4:
                prompt = '%s local files are different from the workspace. Overwrite your local files?' % len(changed_bufs)
            else:
                prompt = 'Overwrite the following local files?\n'
                for buf_id in changed_bufs:
                    prompt += '\n%s' % self.bufs[buf_id]['path']

            stomp_local = yield self.ok_cancel_dialog, prompt
            for buf_id in changed_bufs:
                if stomp_local:
                    self.get_buf(buf_id)
                    self.save_on_get_bufs.add(buf_id)
                else:
                    buf = self.bufs[buf_id]
                    # TODO: this is inefficient. we just read the file 20 lines ago
                    self.upload(utils.get_full_path(buf['path']))

        for buf_id in missing_bufs:
            self.get_buf(buf_id)

        success_msg = 'Successfully joined workspace %s/%s' % (self.owner, self.workspace)
        msg.log(success_msg)
        editor.status_message(success_msg)

        temp_data = data.get('temp_data', {})
        hangout = temp_data.get('hangout', {})
        hangout_url = hangout.get('url')
        if hangout_url:
            self.prompt_join_hangout(hangout_url)

        data = utils.get_persistent_data()
        data['recent_workspaces'].insert(0, {"url": self.workspace_url})
        utils.update_persistent_data(data)
        utils.add_workspace_to_persistent_json(self.owner, self.workspace, self.workspace_url, G.PROJECT_PATH)
        self.emit("room_info")

    def _on_user_info(self, data):
        user_id = str(data['user_id'])
        user_info = data['user_info']
        self.workspace_info['users'][user_id] = user_info
        if user_id == str(self.workspace_info['user_id']):
            G.PERMS = user_info['perms']

    def _on_join(self, data):
        msg.log('%s joined the workspace' % data['username'])
        user_id = str(data['user_id'])
        self.workspace_info['users'][user_id] = data

    def _on_part(self, data):
        msg.log('%s left the workspace' % data['username'])
        user_id = str(data['user_id'])
        try:
            del self.workspace_info['users'][user_id]
        except Exception:
            print('Unable to delete user %s from user list' % (data))

    def _on_set_temp_data(self, data):
        hangout_data = data.get('data', {})
        hangout = hangout_data.get('hangout', {})
        hangout_url = hangout.get('url')
        if hangout_url:
            self.prompt_join_hangout(hangout_url)

    def _on_saved(self, data):
        buf_id = data['id']
        buf = self.bufs.get(buf_id)
        if not buf:
            return
        if G.MIRRORED_SAVES:
            view = self.get_view(data['id'])
            if view:
                self.save_view(view)
            elif 'buf' in buf:
                utils.save_buf(buf)
        username = self.get_username_by_id(data['user_id'])
        msg.log('%s saved buffer %s' % (username, buf['path']))

    @utils.inlined_callbacks
    def _on_request_perms(self, data):
        user_id = str(data.get('user_id'))
        username = self.get_username_by_id(user_id)
        if not username:
            msg.debug('Unknown user for id %s. Not handling request_perms event.' % user_id)
            return

        perm_mapping = {
            'edit_room': 'edit',
            'admin_room': 'admin',
        }
        perms = data.get('perms')
        perms_str = ''.join([perm_mapping.get(p) for p in perms])
        prompt = 'User %s is requesting %s permission for this room.' % (username, perms_str)
        message = data.get('message')
        if message:
            prompt += '\n\n%s says: %s' % (username, message)
        prompt += '\n\nDo you want to grant them permission?'
        confirm = yield self.ok_cancel_dialog, prompt
        self.send({
            'name': 'perms',
            'action': confirm and 'add' or 'reject',
            'user_id': user_id,
            'perms': perms
        })

    def _on_perms(self, data):
        action = data['action']
        user_id = str(data['user_id'])
        user = self.workspace_info['users'].get(user_id)
        if user is None:
            msg.log('No user for id %s. Not handling perms event' % user_id)
            return
        perms = set(user['perms'])
        if action == 'add':
            perms |= set(data['perms'])
        elif action == 'remove':
            perms -= set(data['perms'])
        else:
            return
        user['perms'] = list(perms)
        if user_id == self.workspace_info['user_id']:
            G.PERMS = perms

    def _on_msg(self, data):
        self.on_msg(data)

    @utils.inlined_callbacks
    def upload(self, path, cb=None):
        ig = ignore.Ignore(None, path)
        if ig.size > MAX_WORKSPACE_SIZE:
            size = ig.size
            child_dirs = sorted(ig.children, key=attrgetter("size"))
            ignored_cds = []
            while size > MAX_WORKSPACE_SIZE and child_dirs:
                cd = child_dirs.pop()
                ignored_cds.append(cd)
                size -= cd.size
            if size > MAX_WORKSPACE_SIZE:
                editor.error_message("Maximum workspace size is %.2fMB.\n\n%s is too big (%.2fMB) to upload. Consider adding stuff to the .flooignore file." % (MAX_WORKSPACE_SIZE / 1000000.0, path, ig.size / 1000000.0))
                return
            upload = yield self.ok_cancel_dialog, "Maximum workspace size is %.2fMB.\n\n%s is too big (%.2fMB) to upload.\n\nWould you like to ignore the following and continue?\n\n%s" % \
                (MAX_WORKSPACE_SIZE / 1000000.0, path, ig.size / 1000000.0, "\n".join([x.path for x in ignored_cds]))
            if not upload:
                return
            ig.children = child_dirs
        self._uploader(ig.list_paths(), cb, ig.size)

    def _uploader(self, paths_iter, cb, total_bytes, bytes_uploaded=0.0):
        reactor.tick()
        if len(self.proto) > 0:
            return utils.set_timeout(self._uploader, 10, paths_iter, cb, total_bytes, bytes_uploaded)

        bar_len = 20
        try:
            p = next(paths_iter)
            size = self._upload(p)
            bytes_uploaded += size
            try:
                percent = (bytes_uploaded / total_bytes)
            except ZeroDivisionError:
                percent = 0.5
            bar = '   |' + ('|' * int(bar_len * percent)) + (' ' * int((1 - percent) * bar_len)) + '|'
            editor.status_message('Uploading... %2.2f%% %s' % (percent * 100, bar))
        except StopIteration:
            editor.status_message('Uploading... 100% ' + ('|' * bar_len) + '| complete')
            msg.log('All done uploading')
            return cb and cb()
        return utils.set_timeout(self._uploader, 50, paths_iter, cb, total_bytes, bytes_uploaded)

    def _upload(self, path, text=None):
        size = 0
        try:
            if text:
                # TODO: possible encoding issues with python 3
                buf = text
            else:
                with open(path, 'rb') as buf_fd:
                    buf = buf_fd.read()
            size = len(buf)
            encoding = 'utf8'
            rel_path = utils.to_rel_path(path)
            existing_buf = self.get_buf_by_path(path)
            if existing_buf:
                buf_md5 = hashlib.md5(buf).hexdigest()
                if existing_buf['md5'] == buf_md5:
                    msg.log('%s already exists and has the same md5. Skipping.' % path)
                    return size
                msg.log('Setting buffer ', rel_path)

                try:
                    buf = buf.decode('utf-8')
                except Exception:
                    buf = base64.b64encode(buf).decode('utf-8')
                    encoding = 'base64'

                existing_buf['buf'] = buf
                existing_buf['encoding'] = encoding
                existing_buf['md5'] = buf_md5

                self.send({
                    'name': 'set_buf',
                    'id': existing_buf['id'],
                    'buf': buf,
                    'md5': buf_md5,
                    'encoding': encoding,
                })
                return size

            try:
                buf = buf.decode('utf-8')
            except Exception:
                buf = base64.b64encode(buf).decode('utf-8')
                encoding = 'base64'

            msg.log('Creating buffer ', rel_path)
            event = {
                'name': 'create_buf',
                'buf': buf,
                'path': rel_path,
                'encoding': encoding,
            }
            self.send(event)
        except (IOError, OSError):
            msg.error('Failed to open %s.' % path)
        except Exception as e:
            msg.error('Failed to create buffer %s: %s' % (path, unicode(e)))
        return size
