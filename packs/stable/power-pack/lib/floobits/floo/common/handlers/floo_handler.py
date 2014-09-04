import os
import sys
import hashlib
import base64
import collections
from functools import reduce
from operator import attrgetter

try:
    from . import base
    from ..reactor import reactor
    from ..lib import DMP
    from .. import msg, ignore, shared as G, utils
    from ..exc_fmt import str_e
    from ... import editor
    from ..protocols import floo_proto
except (ImportError, ValueError) as e:
    import base
    from floo import editor
    from floo.common.lib import DMP
    from floo.common.reactor import reactor
    from floo.common.exc_fmt import str_e
    from floo.common import msg, ignore, shared as G, utils
    from floo.common.protocols import floo_proto

try:
    unicode()
except NameError:
    unicode = str

try:
    import io
except ImportError:
    io = None


MAX_WORKSPACE_SIZE = 100000000  # 100MB
TOO_BIG_TEXT = '''Maximum workspace size is %.2fMB.\n
%s is too big (%.2fMB) to upload.\n\nWould you like to ignore the following and continue?\n\n%s'''


class FlooHandler(base.BaseHandler):
    PROTOCOL = floo_proto.FlooProtocol

    def __init__(self, owner, workspace, auth, action):
        self.username = auth.get('username')
        self.secret = auth.get('secret')
        self.api_key = auth.get('api_key')
        # BaseHandler calls reload_settings()
        super(FlooHandler, self).__init__()
        self.owner = owner
        self.workspace = workspace
        self.action = action
        self.reset()

    def _on_highlight(self, data):
        raise NotImplementedError("_on_highlight not implemented")

    def ok_cancel_dialog(self, msg, cb=None):
        raise NotImplementedError("ok_cancel_dialog not implemented.")

    def get_view(self, buf_id):
        raise NotImplementedError("get_view not implemented")

    def get_view_text_by_path(self, rel_path):
        raise NotImplementedError("get_view_text_by_path not implemented")

    def build_protocol(self, *args):
        self.proto = super(FlooHandler, self).build_protocol(*args)

        def f():
            self.joined_workspace = False
        self.proto.on("cleanup", f)
        return self.proto

    def get_username_by_id(self, user_id):
        try:
            return self.workspace_info['users'][str(user_id)]['username']
        except Exception:
            return ''

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
        msg.warn('Syncing buffer ', buf['path'], ' for consistency.')
        if 'buf' in buf:
            del buf['buf']

        if view:
            view.set_read_only(True)
            view.set_status('Floobits locked this file until it is synced.')
            try:
                del G.VIEW_TO_HASH[view.native_id]
            except Exception:
                pass

    def save_view(self, view):
        view.save()

    def on_connect(self):
        utils.reload_settings()

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
        self.on_load = collections.defaultdict(dict)
        self.upload_timeout = None

    def _on_patch(self, data):
        buf_id = data['id']
        buf = self.bufs[buf_id]
        if 'buf' not in buf:
            msg.debug('buf ', buf['path'], ' not populated yet. not patching')
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
                msg.debug('forcing patch for ', buf['path'])
                self.send(patch.to_json())
                old_text = view_text
            else:
                msg.debug('forced patch is true. not sending another force patch for buf ', buf['path'])

        md5_before = hashlib.md5(old_text.encode('utf-8')).hexdigest()
        if md5_before != data['md5_before']:
            msg.warn('starting md5s don\'t match for ', buf['path'], '. this is dangerous!')

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
                    msg.error(e)

            if '\x01' in t[0]:
                msg.debug('FOUND CRAZY BYTE IN BUFFER')
                msg.debug('Starting data:', buf['buf'])
                msg.debug('Patch:', data['patch'])

        timeout_id = buf.get('timeout_id')
        if timeout_id:
            utils.cancel_timeout(timeout_id)
            del buf['timeout_id']

        if not clean_patch:
            msg.log('Couldn\'t patch ', buf['path'], ' cleanly.')
            return self.get_buf(buf_id, view)

        cur_hash = hashlib.md5(t[0].encode('utf-8')).hexdigest()
        if cur_hash != data['md5_after']:
            msg.debug('Ending md5s don\'t match for ', buf['path'], ' Setting get_buf timeout.')
            buf['timeout_id'] = utils.set_timeout(self.get_buf, 2000, buf_id, view)

        buf['buf'] = t[0]
        buf['md5'] = cur_hash

        if not view:
            msg.debug('No view. Not saving buffer ', buf_id)

            def _on_load():
                v = self.get_view(buf_id)
                if v and 'buf' in buf:
                    v.update(buf, message=False)
            self.on_load[buf_id]['patch'] = _on_load
            return

        view.apply_patches(buf, t, data['username'])

    def _on_get_buf(self, data):
        buf_id = data['id']
        buf = self.bufs.get(buf_id)
        if not buf:
            return msg.warn('no buf found: ', data, '. Hopefully you didn\'t need that.')
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
            msg.debug('No view for buf ', buf_id, '. Saving to disk.')
            return utils.save_buf(data)

        view.update(data)
        if save:
            view.save()

    def _on_create_buf(self, data):
        if data['encoding'] == 'base64':
            data['buf'] = base64.b64decode(data['buf'])
        self.bufs[data['id']] = data
        self.paths_to_ids[data['path']] = data['id']
        view = self.get_view(data['id'])
        if view:
            self.save_view(view)
        else:
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
        buf_id = data['id']
        try:
            buf = self.bufs.get(buf_id)
            if buf:
                del self.paths_to_ids[buf['path']]
                del self.bufs[buf_id]
        except KeyError:
            msg.debug('KeyError deleting buf id ', buf_id)
        # TODO: if data['unlink'] == True, add to ignore?
        action = 'removed'
        path = utils.get_full_path(data['path'])
        if data.get('unlink', False):
            action = 'deleted'
            try:
                utils.rm(path)
            except Exception as e:
                msg.debug('Error deleting ', path, ': ', str_e(e))
        user_id = data.get('user_id')
        username = self.get_username_by_id(user_id)
        msg.log(username, ' ', action, ' ', path)

    def _upload_file_by_path(self, rel_path):
        return self._upload(utils.get_full_path(rel_path), self.get_view_text_by_path(rel_path))

    @utils.inlined_callbacks
    def _initial_upload(self, ig, missing_bufs, changed_bufs, cb):
        files, size = yield self.prompt_ignore, ig, G.PROJECT_PATH

        missing_buf_ids = set([buf['id'] for buf in missing_bufs])
        for buf_id in missing_buf_ids:
            self.send({'name': 'delete_buf', 'id': buf_id})

        def __upload_buf(buf):
            return self._upload(utils.get_full_path(buf['path']), buf.get('buf'))

        changed_bufs_len = reduce(lambda a, buf: a + len(buf.get('buf', '')), changed_bufs, 0)
        self._rate_limited_upload(iter(changed_bufs), changed_bufs_len, upload_func=__upload_buf)

        for p, buf_id in self.paths_to_ids.items():
            if p in files:
                files.discard(p)
                continue
            if buf_id in missing_buf_ids:
                continue
            self.send({
                'name': 'delete_buf',
                'id': buf_id,
            })

        def __upload(rel_path):
            buf_id = self.paths_to_ids.get(rel_path)
            text = self.bufs.get(buf_id, {}).get('buf')
            # Only upload stuff that's not in self.bufs (new bufs). We already took care of everything else.
            if text is not None:
                return len(text)
            return self._upload(utils.get_full_path(rel_path), self.get_view_text_by_path(rel_path))

        self._rate_limited_upload(iter(files), size, upload_func=__upload)
        cb()

    @utils.inlined_callbacks
    def _on_room_info(self, data):
        self.reset()
        self.joined_workspace = True
        self.workspace_info = data
        G.PERMS = data['perms']

        self.proto.reset_retries()

        if G.OUTBOUND_FILTERING:
            msg.error('Detected outbound port blocking! See https://floobits.com/help/network for more info.')

        read_only = False
        if 'patch' not in data['perms']:
            read_only = True
            no_perms_msg = '''You don't have permission to edit this workspace. All files will be read-only.'''
            msg.log('No patch permission. Setting buffers to read-only')
            if 'request_perm' in data['perms']:
                should_send = yield self.ok_cancel_dialog, no_perms_msg + '\nDo you want to request edit permission?'
                # TODO: wait for perms to be OK'd/denied before uploading or bailing
                if should_send:
                    self.send({'name': 'request_perms', 'perms': ['edit_room']})
            else:
                if G.EXPERT_MODE:
                    editor.status_message(no_perms_msg)
                else:
                    editor.error_message(no_perms_msg)

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
        utils.update_recent_workspaces(self.workspace_url)

        changed_bufs = []
        missing_bufs = []
        new_files = set()
        ig = ignore.create_ignore_tree(G.PROJECT_PATH)
        G.IGNORE = ig
        if not read_only:
            new_files = set([utils.to_rel_path(x) for x in ig.list_paths()])

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
                buf['buf'] = view_text
                buf['view'] = view
                G.VIEW_TO_HASH[view.native_id] = view_md5
                if view_md5 == buf['md5']:
                    msg.debug('md5 sum matches view. not getting buffer ', buf['path'])
                else:
                    changed_bufs.append(buf)
                    buf['md5'] = view_md5
                continue

            try:
                if buf['encoding'] == 'utf8':
                    if io:
                        buf_fd = io.open(buf_path, 'Urt', encoding='utf8')
                        buf_buf = buf_fd.read()
                    else:
                        buf_fd = open(buf_path, 'rb')
                        buf_buf = buf_fd.read().decode('utf-8').replace('\r\n', '\n')
                    md5 = hashlib.md5(buf_buf.encode('utf-8')).hexdigest()
                else:
                    buf_fd = open(buf_path, 'rb')
                    buf_buf = buf_fd.read()
                    md5 = hashlib.md5(buf_buf).hexdigest()
                buf_fd.close()
                buf['buf'] = buf_buf
                if md5 == buf['md5']:
                    msg.debug('md5 sum matches. not getting buffer ', buf['path'])
                else:
                    msg.debug('md5 differs. possibly getting buffer later ', buf['path'])
                    changed_bufs.append(buf)
                    buf['md5'] = md5
            except Exception as e:
                msg.debug('Error calculating md5 for ', buf['path'], ', ', str_e(e))
                missing_bufs.append(buf)

        ignored = []
        for p, buf_id in self.paths_to_ids.items():
            if p not in new_files:
                ignored.append(p)
            new_files.discard(p)

        if self.action == utils.JOIN_ACTION.UPLOAD:
            yield self._initial_upload, ig, missing_bufs, changed_bufs
            # TODO: maybe use org name here
            who = 'Your friends'
            anon_perms = G.AGENT.workspace_info.get('anon_perms')
            if 'get_buf' in anon_perms:
                who = 'Anyone'
            _msg = 'You are sharing:\n\n%s\n\n%s can join your workspace at:\n\n%s' % (G.PROJECT_PATH, who, G.AGENT.workspace_url)
            # Workaround for horrible Sublime Text bug
            utils.set_timeout(editor.message_dialog, 0, _msg)
        elif changed_bufs or missing_bufs or new_files:
            # TODO: handle readonly here
            if self.action == utils.JOIN_ACTION.PROMPT:
                stomp_local = yield self.stomp_prompt, changed_bufs, missing_bufs, list(new_files), ignored
                if stomp_local not in [0, 1]:
                    self.stop()
                    return
            elif self.action == utils.JOIN_ACTION.DOWNLOAD:
                stomp_local = True
            else:
                # This should never happen
                assert False
                return

            if stomp_local:
                for buf in changed_bufs:
                    self.get_buf(buf['id'], buf.get('view'))
                    self.save_on_get_bufs.add(buf['id'])
                for buf in missing_bufs:
                    self.get_buf(buf['id'], buf.get('view'))
                    self.save_on_get_bufs.add(buf['id'])
            else:
                yield self._initial_upload, ig, missing_bufs, changed_bufs

        success_msg = 'Successfully joined workspace %s/%s' % (self.owner, self.workspace)
        msg.log(success_msg)
        editor.status_message(success_msg)

        data = utils.get_persistent_data()
        data['recent_workspaces'].insert(0, {"url": self.workspace_url})
        utils.update_persistent_data(data)
        utils.add_workspace_to_persistent_json(self.owner, self.workspace, self.workspace_url, G.PROJECT_PATH)

        temp_data = data.get('temp_data', {})
        hangout = temp_data.get('hangout', {})
        hangout_url = hangout.get('url')
        if hangout_url:
            self.prompt_join_hangout(hangout_url)

        self.emit("room_info")

    def _on_user_info(self, data):
        user_id = str(data['user_id'])
        user_info = data['user_info']
        self.workspace_info['users'][user_id] = user_info
        if user_id == str(self.workspace_info['user_id']):
            G.PERMS = user_info['perms']

    def _on_join(self, data):
        msg.log(data['username'], ' joined the workspace')
        user_id = str(data['user_id'])
        self.workspace_info['users'][user_id] = data

    def _on_part(self, data):
        msg.log(data['username'], ' left the workspace')
        user_id = str(data['user_id'])
        try:
            del self.workspace_info['users'][user_id]
        except Exception:
            msg.error('Unable to delete user %s from user list' % (data))

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
        on_view_load = self.on_load.get(buf_id)
        if on_view_load:
            try:
                del on_view_load['patch']
            except KeyError:
                pass
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
            msg.debug('Unknown user for id ', user_id, '. Not handling request_perms event.')
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
            'perms': perms,
        })

    def _on_perms(self, data):
        action = data['action']
        user_id = str(data['user_id'])
        user = self.workspace_info['users'].get(user_id)
        if user is None:
            msg.log('No user for id ', user_id, '. Not handling perms event')
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

    def _on_ping(self, data):
        self.send({'name': 'pong'})

    @utils.inlined_callbacks
    def prompt_ignore(self, ig, path, cb):
        ignore.create_flooignore(ig.path)
        dirs = ig.get_children()
        dirs.append(ig)
        dirs = sorted(dirs, key=attrgetter('size'))
        size = starting_size = reduce(lambda x, c: x + c.size, dirs, 0)
        too_big = []
        while size > MAX_WORKSPACE_SIZE and dirs:
            cd = dirs.pop()
            size -= cd.size
            too_big.append(cd)
        if size > MAX_WORKSPACE_SIZE:
            editor.error_message(
                'Maximum workspace size is %.2fMB.\n\n%s is too big (%.2fMB) to upload. Consider adding stuff to the .flooignore file.' %
                (MAX_WORKSPACE_SIZE / 1000000.0, path, ig.size / 1000000.0))
            cb([set(), 0])
            return
        if too_big:
            txt = TOO_BIG_TEXT % (MAX_WORKSPACE_SIZE / 1000000.0, path, starting_size / 1000000.0, "\n".join(set([x.path for x in too_big])))
            upload = yield self.ok_cancel_dialog, txt
            if not upload:
                cb([set(), 0])
                return
        files = set()
        for ig in dirs:
            files = files.union(set([utils.to_rel_path(x) for x in ig.files]))
        cb([files, size])

    def upload(self, path):
        if not utils.is_shared(path):
            editor.error_message('Cannot share %s because is not in shared path %s.\n\nPlease move it there and try again.' % (path, G.PROJECT_PATH))
            return
        ig = ignore.create_ignore_tree(G.PROJECT_PATH)
        G.IGNORE = ig
        is_dir = os.path.isdir(path)
        if ig.is_ignored(path, is_dir, True):
            editor.error_message('Cannot share %s because it is ignored.\n\nAdd an exclude rule (!%s) to your .flooignore file.' % (path, path))
            return
        rel_path = utils.to_rel_path(path)
        if not is_dir:
            self._upload_file_by_path(rel_path)
            return

        for p in rel_path.split('/'):
            child = ig.children.get(p)
            if not child:
                break
            ig = child

        if ig.path != path:
            msg.warn(ig.path, ' is not the same as ', path)

        self._rate_limited_upload(ig.list_paths(), ig.total_size, upload_func=self._upload_file_by_path)

    def _rate_limited_upload(self, paths_iter, total_bytes, bytes_uploaded=0.0, upload_func=None):
        reactor.tick()
        upload_func = upload_func or (lambda x: self._upload(utils.get_full_path(x)))
        if len(self.proto) > 0:
            self.upload_timeout = utils.set_timeout(self._rate_limited_upload, 10, paths_iter, total_bytes, bytes_uploaded, upload_func)
            return

        bar_len = 20
        try:
            p = next(paths_iter)
            size = upload_func(p)
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
            return
        self.upload_timeout = utils.set_timeout(self._rate_limited_upload, 50, paths_iter, total_bytes, bytes_uploaded, upload_func)

    def _upload(self, path, text=None):
        size = 0
        try:
            if text is None:
                with open(path, 'rb') as buf_fd:
                    buf = buf_fd.read()
            else:
                try:
                    # work around python 3 encoding issue
                    buf = text.encode('utf8')
                except Exception as e:
                    msg.debug('Error encoding buf ', path, ': ', str_e(e))
                    # We're probably in python 2 so it's ok to do this
                    buf = text
            size = len(buf)
            encoding = 'utf8'
            rel_path = utils.to_rel_path(path)
            existing_buf = self.get_buf_by_path(path)
            if existing_buf:
                if text is None:
                    buf_md5 = hashlib.md5(buf).hexdigest()
                    if existing_buf['md5'] == buf_md5:
                        msg.log(path, ' already exists and has the same md5. Skipping.')
                        return size
                    existing_buf['md5'] = buf_md5
                msg.log('Setting buffer ', rel_path)

                try:
                    buf = buf.decode('utf-8')
                except Exception:
                    buf = base64.b64encode(buf).decode('utf-8')
                    encoding = 'base64'

                existing_buf['buf'] = buf
                existing_buf['encoding'] = encoding

                self.send({
                    'name': 'set_buf',
                    'id': existing_buf['id'],
                    'buf': buf,
                    'md5': existing_buf['md5'],
                    'encoding': encoding,
                })
                return size

            try:
                buf = buf.decode('utf-8')
            except Exception:
                buf = base64.b64encode(buf).decode('utf-8')
                encoding = 'base64'

            msg.log('Creating buffer ', rel_path, ' (', len(buf), ' bytes)')
            event = {
                'name': 'create_buf',
                'buf': buf,
                'path': rel_path,
                'encoding': encoding,
            }
            self.send(event)
        except (IOError, OSError):
            msg.error('Failed to open ', path)
        except Exception as e:
            msg.error('Failed to create buffer ', path, ': ', str_e(e))
        return size

    def stop(self):
        if self.upload_timeout is not None:
            utils.cancel_timeout(self.upload_timeout)
            self.upload_timeout = None

        super(FlooHandler, self).stop()
