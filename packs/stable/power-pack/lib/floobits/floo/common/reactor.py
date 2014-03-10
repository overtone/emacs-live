import socket
import select

try:
    from . import msg
    from .. import editor
    from ..common.handlers import tcp_server
    assert msg and tcp_server
except (ImportError, ValueError):
    from floo.common.handlers import tcp_server
    from floo import editor
    import msg

reactor = None


class _Reactor(object):
    ''' Low level event driver '''
    def __init__(self):
        self._protos = []
        self._handlers = []

    def connect(self, factory, host, port, secure, conn=None):
        proto = factory.build_protocol(host, port, secure)
        self._protos.append(proto)
        proto.connect(conn)
        self._handlers.append(factory)

    def listen(self, factory, host='127.0.0.1', port=0):
        listener_factory = tcp_server.TCPServerHandler(factory, self)
        proto = listener_factory.build_protocol(host, port)
        self._protos.append(proto)
        self._handlers.append(listener_factory)
        return proto.sockname()

    def stop(self):
        for _conn in self._protos:
            _conn.stop()

        self._protos = []
        self._handlers = []
        msg.log('Disconnected.')
        editor.status_message('Disconnected.')

    def is_ready(self):
        if not self._handlers:
            return False
        for f in self._handlers:
            if not f.is_ready():
                return False
        return True

    def _reconnect(self, fd, *fd_sets):
        for fd_set in fd_sets:
            try:
                fd_set.remove(fd)
            except ValueError:
                pass
        fd.reconnect()

    def tick(self, timeout=0):
        for factory in self._handlers:
            factory.tick()
        self.select(timeout)
        editor.call_timeouts()

    def block(self):
        while True:
            self.tick(.05)

    def select(self, timeout=0):
        if not self._handlers:
            return

        readable = []
        writeable = []
        errorable = []
        fd_map = {}

        for fd in self._protos:
            fileno = fd.fileno()
            if not fileno:
                continue
            fd.fd_set(readable, writeable, errorable)
            fd_map[fd.fileno()] = fd

        if not readable and not writeable:
            return

        try:
            _in, _out, _except = select.select(readable, writeable, errorable, timeout)
        except (select.error, socket.error, Exception) as e:
            # TODO: with multiple FDs, must call select with just one until we find the error :(
            if len(readable) == 1:
                readable[0].reconnect()
                return msg.error('Error in select(): %s' % str(e))
            raise Exception("can't handle more than one fd exception in reactor")

        for fileno in _except:
            fd = fd_map[fileno]
            self._reconnect(fd, _in, _out)

        for fileno in _out:
            fd = fd_map[fileno]
            try:
                fd.write()
            except Exception as e:
                msg.error('Couldn\'t write to socket: %s' % str(e))
                return self._reconnect(fd, _in)

        for fileno in _in:
            fd = fd_map[fileno]
            try:
                fd.read()
            except Exception as e:
                msg.error('Couldn\'t read from socket: %s' % str(e))
                fd.reconnect()

reactor = _Reactor()
