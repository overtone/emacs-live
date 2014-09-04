# coding: utf-8

try:
    from . import agent_connection
    from .common import flooui
except (ImportError, ValueError):
    import agent_connection
    from floo.common import flooui


class Emui(flooui.FlooUI):
    def __init__(self):
        super(Emui, self).__init__()
        self.user_inputs = {}
        self.user_input_count = 0

    def _make_agent(self, context, owner, workspace, auth, join_action):
        """@returns new Agent()"""
        return agent_connection.AgentConnection(owner, workspace, context, auth, join_action)

    def _send_input(self, context, event, cb):
        self.user_input_count += 1
        self.user_inputs[self.user_input_count] = cb
        event['id'] = self.user_input_count
        event['name'] = 'user_input'
        context.send(event)

    def user_dir(self, context, prompt, initial, cb):
        self._send_input(context, {'prompt': prompt, 'initial': initial, 'dir': True}, cb)

    def user_y_or_n(self, context, prompt, affirmation_txt, cb):
        """@returns True/False"""
        event = {'prompt': prompt.replace('\n', ', ').replace(", ,", "") + '? ', 'initial': "", 'y_or_n': True}
        self._send_input(context, event, cb)

    def user_select(self, context, prompt, choices_big, choices_small, cb):
        """@returns (choice, index)"""
        choices = [["%d. %s" % (i + 1, v), i] for i, v in enumerate(choices_big)]
        event = {
            'choices': choices,
            'initial': '',
            'prompt': prompt + "\n\n%s\n\nPlease select an option: " % "\n".join([c[0] for c in choices])
        }

        def _cb(choice):
            if not choice:
                return cb(None, -1)
            c = int(choice[:choice.index(".")]) - 1
            print(c, choices_big[c], choice)
            return cb(choices_big[c], c)

        self._send_input(context, event, _cb)

    def user_charfield(self, context, prompt, initial, cb):
        """@returns String"""
        self._send_input(context, {'prompt': prompt, 'initial': initial}, cb)

    def get_a_window(self, abs_path, cb):
        """opens a project in a window or something"""
        return cb()

    def on_user_input(self, cb_id, res):
        cb = self.user_inputs.get(cb_id)
        if cb is None:
            print('cb for input %s is none' % cb_id)
            return
        cb(res)
        del self.user_inputs[cb_id]
