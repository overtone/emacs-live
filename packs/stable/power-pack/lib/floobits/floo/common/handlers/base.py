try:
    from ... import editor
except ValueError:
    from floo import editor
from .. import msg, event_emitter, shared as G, utils


class BaseHandler(event_emitter.EventEmitter):
    PROTOCOL = None

    def __init__(self):
        super(BaseHandler, self).__init__()
        self.joined_workspace = False
        G.AGENT = self
        # TODO: removeme?
        utils.reload_settings()
        self.req_ids = {}

    def build_protocol(self, *args):
        self.proto = self.PROTOCOL(*args)
        self.proto.on('data', self.on_data)
        self.proto.on('connect', self.on_connect)
        return self.proto

    def send(self, d):
        """@return the request id"""
        if not d:
            return
        req_id = self.proto.put(d)

        name = d.get('name', '?')
        if name != "pong":
            self.req_ids[req_id] = name

        return req_id

    def on_data(self, name, data):
        req_id = data.get('res_id')
        if req_id is not None:
            try:
                del self.req_ids[req_id]
            except KeyError:
                msg.warn('No outstanding req_id ', req_id)

        handler = getattr(self, '_on_%s' % name, None)
        if handler:
            return handler(data)
        msg.debug('unknown event name ', name, ' data: ', data)

    @property
    def client(self):
        return editor.name()

    @property
    def codename(self):
        return editor.codename()

    def _on_ack(self, data):
        msg.debug('Ack ', data)

    def _on_error(self, data):
        message = 'Error from Floobits server: %s' % str(data.get('msg'))
        msg.error(message)
        if data.get('flash'):
            editor.error_message(message)

    def _on_disconnect(self, data):
        message = 'Disconnected from server! Reason: %s' % str(data.get('reason'))
        msg.error(message)
        editor.error_message(message)
        self.stop()

    def stop(self):
        from .. import reactor
        if self.req_ids:
            msg.warn("Unresponded msgs", self.req_ids)
            self.req_ids = {}
        reactor.reactor.stop_handler(self)
        if G.AGENT is self:
            G.AGENT = None

    def is_ready(self):
        return self.joined_workspace

    def tick(self):
        pass
