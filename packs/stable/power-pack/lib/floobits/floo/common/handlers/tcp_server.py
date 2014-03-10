
try:
    from . import base
    from .. protocols import tcp_server
except (ImportError, ValueError):
    from floo.common.protocols import tcp_server
    import base


class TCPServerHandler(base.BaseHandler):
    PROTOCOL = tcp_server.TCPServerProtocol

    def __init__(self, factory, reactor):
        self.factory = factory
        self.reactor = reactor

    def is_ready(self):
        return True

    def on_connect(self, conn, host, port):
        self.reactor.connect(self.factory, host, port, False, conn)
