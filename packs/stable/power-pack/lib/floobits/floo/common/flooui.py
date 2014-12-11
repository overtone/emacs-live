import os.path
import webbrowser
import re
import json

try:
    from . import api, msg, utils, reactor, shared as G, event_emitter
    from .handlers import account, credentials
    from .. import editor
    from ..common.exc_fmt import str_e
except (ImportError, ValueError):
    from floo.common.exc_fmt import str_e
    from floo.common.handlers import account, credentials
    from floo.common import api, msg, utils, reactor, shared as G, event_emitter
    from floo import editor


class FlooUI(event_emitter.EventEmitter):
    def __init__(self):
        super(FlooUI, self).__init__()
        self.agent = None

    def _make_agent(self, context, owner, workspace, auth, join_action):
        """@returns new Agent()"""
        raise NotImplemented()

    def user_y_or_n(self, context, prompt, affirmation_txt, cb):
        """@returns True/False"""
        raise NotImplemented()

    def user_select(self, context, prompt, choices_big, choices_small, cb):
        """@returns (choice, index)"""
        raise NotImplemented()

    def user_charfield(self, context, prompt, initial, cb):
        """@returns String"""
        raise NotImplemented()

    def user_dir(self, context, prompt, initial, cb):
        """@returns a String directory (probably not expanded)"""
        raise NotImplemented()

    def get_a_window(self, abs_path, cb):
        """opens a project in a window or something"""
        raise NotImplemented()

    @utils.inlined_callbacks
    def link_account(self, context, host, cb):
        prompt = 'No credentials found in ~/.floorc.json for %s. Would you like to sign in? (opens a browser)' % host
        yes = yield self.user_y_or_n, context, prompt, 'Sign in'
        if not yes:
            return

        agent = credentials.RequestCredentialsHandler()
        if not agent:
            self.error_message('''A configuration error occured earlier. Please go to %s and sign up to use this plugin.\n
    We're really sorry. This should never happen.''' % host)
            return

        agent.once('end', cb)

        try:
            reactor.reactor.connect(agent, host, G.DEFAULT_PORT, True)
        except Exception as e:
            print(str_e(e))

    @utils.inlined_callbacks
    def create_or_link_account(self, context, host, force, cb):
        if host != "floobits.com":
            self.link_account(context, host, cb)
            return
        disable_account_creation = utils.get_persistent_data().get('disable_account_creation')
        if disable_account_creation and not force:
            print('We could not automatically create or link your floobits account. Please go to floobits.com and sign up to use this plugin.')
            return

        if not G.EXPERT_MODE:
            editor.message_dialog('Thank you for installing the Floobits plugin!\n\nLet\'s set up your editor to work with Floobits.')

        choices = [
            'Sign in to Floobits',
            'Create a Floobits account',
            'Cancel (see https://floobits.com/help/floorc)'
        ]

        (choice, index) = yield self.user_select, context, 'You need an account to use Floobits! Do you want to:', choices, None

        if index == -1 or index == 2:
            d = utils.get_persistent_data()
            if not d.get('disable_account_creation'):
                d['disable_account_creation'] = True
                utils.update_persistent_data(d)
                # TODO: this instruction is only useful for Sublime Text
                editor.message_dialog('''You can set up a Floobits account at any time under\n\nTools -> Floobits -> Set up''')
            cb(None)
            return

        agent = None
        if index == 0:
            agent = credentials.RequestCredentialsHandler()
        else:
            agent = account.CreateAccountHandler()

        agent.once('end', cb)

        try:
            reactor.reactor.connect(agent, host, G.DEFAULT_PORT, True)
        except Exception as e:
            print(str_e(e))

    def open_workspace(self):
        if not self.agent:
            return
        try:
            webbrowser.open(self.agent.workspace_url, new=2, autoraise=True)
        except Exception as e:
            msg.error("Couldn't open a browser: %s" % (str_e(e)))

    def open_workspace_settings(self):
        if not self.agent:
            return
        try:
            webbrowser.open(self.agent.workspace_url + '/settings', new=2, autoraise=True)
        except Exception as e:
            msg.error("Couldn't open a browser: %s" % (str_e(e)))

    def pinocchio(self, host=None):
        floorc = utils.load_floorc_json()
        auth = floorc.get('AUTH', {}).get(host or G.DEFAULT_HOST, {})
        username = auth.get('username')
        secret = auth.get('secret')
        if not (username and secret):
            return self.error_message('You don\'t seem to have a Floobits account of any sort')
        webbrowser.open('https://%s/%s/pinocchio/%s' % (G.DEFAULT_HOST, username, secret))

    def prejoin_workspace(self, workspace_url, dir_to_share, api_args):
        try:
            result = utils.parse_url(workspace_url)
        except Exception as e:
            msg.error(str_e(e))
            return False

        host = result.get('host')
        if not api.get_basic_auth(host):
            raise ValueError('No auth credentials for %s. Please add a username and secret for %s in your ~/.floorc.json' % (host, host))

        try:
            w = api.get_workspace_by_url(workspace_url)
        except Exception as e:
            editor.error_message('Error opening url %s: %s' % (workspace_url, str_e(e)))
            return False

        if w.code >= 400:
            try:
                d = utils.get_persistent_data()
                try:
                    del d['workspaces'][result['owner']][result['name']]
                except Exception:
                    pass
                try:
                    del d['recent_workspaces'][workspace_url]
                except Exception:
                    pass
                utils.update_persistent_data(d)
            except Exception as e:
                msg.debug(str_e(e))
            return False

        msg.debug('workspace: ', json.dumps(w.body))
        anon_perms = w.body.get('perms', {}).get('AnonymousUser', [])
        msg.debug('api args: ', api_args)
        new_anon_perms = api_args.get('perms', {}).get('AnonymousUser', [])
        # TODO: prompt/alert user if going from private to public
        if set(anon_perms) != set(new_anon_perms):
            msg.debug(str(anon_perms), str(new_anon_perms))
            w.body['perms']['AnonymousUser'] = new_anon_perms
            response = api.update_workspace(workspace_url, w.body)
            msg.debug(str(response.body))
        utils.add_workspace_to_persistent_json(w.body['owner'], w.body['name'], workspace_url, dir_to_share)
        return result

    @utils.inlined_callbacks
    def remote_connect(self, context, host, owner, workspace, d, join_action=utils.JOIN_ACTION.PROMPT):
        G.PROJECT_PATH = os.path.realpath(d)
        try:
            utils.mkdir(os.path.dirname(G.PROJECT_PATH))
        except Exception as e:
            msg.error("Couldn't create directory", G.PROJECT_PATH, str_e(e))
            return

        auth = G.AUTH.get(host)
        if not auth:
            success = yield self.link_account, context, host
            if not success:
                return
            auth = G.AUTH.get(host)
            if not auth:
                msg.error("Something went really wrong.")
                return

        res = api.get_workspace(host, owner, workspace)
        if res.code == 404:
            msg.error("The workspace https://%s/%s/%s does not exist" % (host, owner, workspace))
            return

        if self.agent:
            try:
                self.agent.stop()
            except:
                pass

        G.WORKSPACE_WINDOW = yield self.get_a_window, d
        self.agent = self._make_agent(context, owner, workspace, auth, join_action)
        self.emit("agent", self.agent)
        reactor.reactor.connect(self.agent, host, G.DEFAULT_PORT, True)
        url = self.agent.workspace_url
        utils.add_workspace_to_persistent_json(owner, workspace, url, d)
        utils.update_recent_workspaces(url)

    @utils.inlined_callbacks
    def create_workspace(self, context, host, owner, name, api_args, dir_to_share):
        prompt = 'Workspace name: '

        api_args['name'] = name
        api_args['owner'] = owner

        while True:
            new_name = yield self.user_charfield, context, prompt, name
            name = new_name or name
            try:
                api_args['name'] = name
                r = api.create_workspace(host, api_args)
            except Exception as e:
                msg.error('Unable to create workspace ', str_e(e))
                editor.error_message('Unable to create workspace: %s' % str_e(e))
                return

            if r.code < 400:
                workspace_url = 'https://%s/%s/%s' % (host, owner, name)
                msg.log('Created workspace ', workspace_url)
                self.remote_connect(context, host, owner, name, dir_to_share, utils.JOIN_ACTION.UPLOAD)
                return

            msg.error('Unable to create workspace: ', r.body)

            if r.code not in (400, 402, 409):
                try:
                    r.body = r.body['detail']
                except Exception:
                    pass
                editor.error_message('Unable to create workspace: %s' % r.body)
                return

            if r.code == 402:
                try:
                    r.body = r.body['detail']
                except Exception:
                    pass

                yes = yield self.user_y_or_n, context, '%s Open billing settings?' % r.body, "Yes"
                if yes:
                    webbrowser.open('https://%s/%s/settings#billing' % (host, owner))
                return

            if r.code == 400:
                # TODO: strip leading dots/dashes/etc
                name = re.sub('[^A-Za-z0-9_\-\.]', '_', name)
                prompt = 'Workspace names may only contain [A-Za-z0-9_\-\.]. Choose another name: '
                continue

            prompt = 'Workspace %s/%s already exists. Choose another name: ' % (owner, name)

    def join_workspace_by_url(self, context, workspace_url, possible_dirs=None):
        try:
            d = utils.parse_url(workspace_url)
        except Exception as e:
            return editor.error_message(str_e(e))

        return self.join_workspace(context, d['host'], d['workspace'], d['owner'], possible_dirs)

    @utils.inlined_callbacks
    def follow_user(self, context):
        users = self.agent.workspace_info.get('users')
        userNames = set()
        me = self.agent.get_username_by_id(self.agent.workspace_info['user_id'])
        for user in users.values():
            username = user['username']
            if username == me:
                continue
            if user['client'] == 'flooty':
                continue
            if 'highlight' not in user['perms']:
                continue
            userNames.add(username)
        if not userNames:
            editor.error_message("There are no other users that can be followed at this time.  NOTE: you can only follow users who have highlight permission.")
            return
        userNames = list(userNames)
        userNames.sort()
        small = [(x in G.FOLLOW_USERS) and "unfollow" or "follow" for x in userNames]
        selected_user, index = yield self.user_select, context, "select a user to follow", list(userNames), small

        if not selected_user:
            return

        if selected_user in G.FOLLOW_USERS:
            G.FOLLOW_USERS.remove(selected_user)
            return

        G.FOLLOW_USERS.add(selected_user)
        G.AGENT.highlight(user=selected_user)

    @utils.inlined_callbacks
    def join_workspace(self, context, host, name, owner, possible_dirs=None):
        utils.reload_settings()

        # legacy urls in emacs...
        if owner and owner[:2] == "r/":
            owner = owner[2:]

        if not utils.can_auth():
            success = yield self.create_or_link_account, context, host, False
            if not success:
                return
            utils.reload_settings()

        possible_dirs = possible_dirs or []
        for d in possible_dirs:
            info = utils.read_floo_file(d)
            if not info:
                continue
            try:
                parsed_url = utils.parse_url(info['url'])
            except Exception:
                parsed_url = None

            if parsed_url and parsed_url['host'] == host and parsed_url['workspace'] == name and parsed_url['owner'] == owner:
                self.remote_connect(context, host, owner, name, d)
                return

        try:
            d = utils.get_persistent_data()['workspaces'][owner][name]['path']
        except Exception:
            d = ''

        if d and os.path.isdir(d):
            self.remote_connect(context, host, owner, name, d)
            return

        d = d or os.path.join(G.SHARE_DIR or G.BASE_DIR, owner, name)
        join_action = utils.JOIN_ACTION.PROMPT
        while True:
            d = yield self.user_dir, context, 'Save workspace files to: ', d
            if not d:
                return
            d = os.path.realpath(os.path.expanduser(d))
            if not os.path.isdir(d):
                y_or_n = yield self.user_y_or_n, context, '%s is not a directory. Create it? ' % d, "Create Directory"
                if not y_or_n:
                    return
                utils.mkdir(d)
                if not os.path.isdir(d):
                    msg.error("Couldn't create directory", d)
                    continue
                join_action = utils.JOIN_ACTION.DOWNLOAD
            if os.path.isdir(d):
                self.remote_connect(context, host, owner, name, d, join_action)
                return

    @utils.inlined_callbacks
    def prompt_share_dir(self, context, ask_about_dir, api_args):
        dir_to_share = yield self.user_dir, context, 'Directory to share: ', ask_about_dir
        if not dir_to_share:
            return
        self.share_dir(context, dir_to_share, api_args)

    @utils.inlined_callbacks
    def share_dir(self, context, dir_to_share, api_args):
        utils.reload_settings()
        if not utils.can_auth():
            success = yield self.create_or_link_account, context, G.DEFAULT_HOST, False
            if not success:
                return
            utils.reload_settings()

        dir_to_share = os.path.expanduser(dir_to_share)
        dir_to_share = os.path.realpath(dir_to_share)
        dir_to_share = utils.unfuck_path(dir_to_share)

        if os.path.isfile(dir_to_share):
            dir_to_share = os.path.dirname(dir_to_share)

        workspace_name = os.path.basename(dir_to_share)
        msg.debug('', workspace_name, dir_to_share)

        if os.path.isfile(dir_to_share):
            dir_to_share = os.path.dirname(dir_to_share)

        try:
            utils.mkdir(dir_to_share)
        except Exception:
            msg.error("The directory", dir_to_share, "doesn't exist and I can't create it.")
            return

        info = utils.read_floo_file(dir_to_share)

        def prejoin(workspace_url):
            try:
                return self.prejoin_workspace(workspace_url, dir_to_share, api_args)
            except ValueError:
                pass

        workspace_url = info.get('url')
        if workspace_url:
            parsed_url = prejoin(workspace_url)
            if parsed_url:
                self.remote_connect(context, parsed_url['host'], parsed_url['owner'], parsed_url['workspace'], dir_to_share)
                return

        parsed_url = utils.get_workspace_by_path(dir_to_share, prejoin)
        if parsed_url:
            self.remote_connect(context, parsed_url['host'], parsed_url['owner'], parsed_url['workspace'], dir_to_share)
            return

        host = yield self._get_host, context
        if not host:
            return

        try:
            r = api.get_orgs_can_admin(host)
        except IOError as e:
            editor.error_message('Error getting org list: %s' % str_e(e))
            return

        choices = [G.AUTH[host]['username']]
        if r.code >= 400:
            editor.error_message('Error getting org list: %s' % r.body)
        elif r.body:
            choices += [org['name'] for org in r.body]

        if len(choices) == 1:
            owner = choices[0]
        else:
            little = ['Create workspace owned by %s' % s for s in choices]
            (owner, index) = yield self.user_select, context, 'Create workspace owned by', choices, little

        if not owner:
            return

        self.create_workspace(context, host, owner, workspace_name, api_args, dir_to_share)

    @utils.inlined_callbacks
    def _get_host(self, context, cb):
        if not G.AUTH:
            msg.warn('no auth')
            return

        hosts = list(G.AUTH.keys())
        if len(hosts) == 1:
            host = hosts[0]
        else:
            little = ["%s on %s" % (a['username'], h) for h, a in G.AUTH.items()]
            (host, index) = yield self.user_select, context, 'Which Floobits account should be used?', hosts, little
            if not host:
                cb(None)
                return
        cb(host)

    @utils.inlined_callbacks
    def delete_workspace(self, context, cb):
        host = yield self._get_host, context
        if not host:
            cb()
            return

        api_url = 'https://%s/api/workspaces/can/admin' % (host)

        try:
            r = api.api_request(host, api_url)
        except IOError as e:
            editor.error_message('Error getting workspaces can admin %s' % str_e(e))
            cb()
            return

        if r.code >= 400:
            editor.error_message('Error getting workspace list: %s' % r.body)
            cb()
            return

        choices = ['%s/%s' % (workspace['owner'], workspace['name']) for workspace in r.body]
        (workspace, index) = yield self.user_select, context, 'Select workpace to delete', choices, []

        if not workspace:
            cb()
            return

        if G.EXPERT_MODE:
            yes = True
        else:
            yes = yield self.user_y_or_n, context, 'Really delete %s?' % workspace, 'Yes'

        if not yes:
            cb()
            return

        workspace = r.body[index]

        try:
            api.delete_workspace(host, workspace['owner'], workspace['name'])
        except IOError as e:
            editor.error_message('Error deleting workspace' % str_e(e))
        cb()
