try:
    from ... import editor
except ValueError:
    from floo import editor
from .. import msg, event_emitter, shared as G, utils


BASE_FLOORC = '''# Floobits config

# Logs messages to Sublime Text console instead of a special view
#log_to_console 1

# Enables debug mode
#debug 1

'''


class BaseHandler(event_emitter.EventEmitter):
    BASE_FLOORC = BASE_FLOORC
    PROTOCOL = None

    def __init__(self):
        super(BaseHandler, self).__init__()
        G.AGENT = self

    def build_protocol(self, *args):
        self.proto = self.PROTOCOL(*args)
        self.proto.on("data", self.on_data)
        self.proto.on("connect", self.on_connect)
        return self.proto

    def send(self, *args, **kwargs):
        self.proto.put(*args, **kwargs)

    def on_data(self, name, data):
        handler = getattr(self, "_on_%s" % name, None)
        if handler:
            return handler(data)
        msg.debug('unknown name!', name, 'data:', data)

    @property
    def client(self):
        return editor.name()

    def _on_error(self, data):
        message = 'Floobits: Error! Message: %s' % str(data.get('msg'))
        msg.error(message)
        if data.get('flash'):
            editor.error_message('Floobits: %s' % str(data.get('msg')))

    def _on_disconnect(self, data):
        message = 'Floobits: Disconnected! Reason: %s' % str(data.get('reason'))
        msg.error(message)
        editor.error_message(message)
        self.proto.stop()

    def is_ready(self):
        return G.JOINED_WORKSPACE

    def reload_settings(self):
        utils.reload_settings()
        self.username = G.USERNAME
        self.secret = G.SECRET
        self.api_key = G.API_KEY

    def tick(self):
        pass
