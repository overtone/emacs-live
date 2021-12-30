# coding: utf-8
import sys

try:
    from . import shared as G, utils, reactor
    from .handlers import base
    from .protocols import floo_proto
except (ImportError, ValueError):
    import msg
    import shared as G
    import reactor
    from handlers import base
    from protocols import floo_proto


# KANS: this should use base, but I want the connection logic from FlooProto (ie, move that shit to base)
class ProxiedProtocol(floo_proto.FlooProtocol):
    ''' Speaks floo proto, but is given the conn and we don't want to reconnect '''
    def _handle(self, data):
        self.proxy(data)


class FlooConn(base.BaseHandler):
    PROTOCOL = ProxiedProtocol

    def __init__(self, server):
        super(ProxyServer, self).__init__()
        self.proxy = server.send  # agent handler (to the backend connection)

    def tick(self):
        pass

    def on_connect(self):
        msg.log('Connection established.')
        self.proto.proxy = self.proxy


class ProxyProtocol(floo_proto.FlooProtocol):
    ''' Speaks floo proto, but is given the conn and we don't want to reconnect '''
    MAX_RETRIES = -1
    INITIAL_RECONNECT_DELAY = 0

    def connect(self, sock=None):
        self.emit('connected')
        self._sock = sock
        self.connected = True

    def reconnect(self):
        msg.error('Client connection died!')
        sys.exit(1)

    def stop(self):
        self.cleanup()


class ProxyServer(base.BaseHandler):
    PROTOCOL = ProxyProtocol

    def on_connect(self):
        msg.log('Connection established.')
        reactor.reactor.connect(FlooConn(self), G.DEFAULT_HOST, G.DEFAULT_PORT, True)


def main():
    G.__VERSION__ = '0.11'
    G.__PLUGIN_VERSION__ = '1.0'
    utils.reload_settings()

    floo_log_level = 'msg'
    if G.DEBUG:
        floo_log_level = 'debug'
    msg.LOG_LEVEL = msg.LOG_LEVELS.get(floo_log_level.upper(), msg.LOG_LEVELS['MSG'])

    proxy = ProxyServer()
    _, port = reactor.reactor.listen(proxy)

    def on_ready():
        print('Now listening on %s' % port)

    utils.set_timeout(on_ready, 100)
    reactor.reactor.block()


if __name__ == '__main__':
    main()
