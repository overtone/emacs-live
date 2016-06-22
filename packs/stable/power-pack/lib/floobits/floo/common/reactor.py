import socket
import select

try:
    from . import api, msg
    from .. import editor
    from ..common.exc_fmt import str_e, pp_e
    from ..common.handlers import tcp_server
    assert msg and tcp_server
except (ImportError, ValueError):
    from floo.common.exc_fmt import str_e, pp_e
    from floo.common.handlers import tcp_server
    from floo.common import api, msg
    from floo import editor

reactor = None


class _Reactor(object):
    ''' Low level event driver '''
    def __init__(self):
        self._protos = []
        self._handlers = []
        self.on_stop = None

    def connect(self, factory, host, port, secure, conn=None):
        proto = factory.build_protocol(host, port, secure)
        self._protos.append(proto)
        proto.connect(conn)
        self._handlers.append(factory)

    def listen(self, factory, host='127.0.0.1', port=0):
        listener_factory = tcp_server.TCPServerHandler(factory, self)
        proto = listener_factory.build_protocol(host, port)
        factory.listener_factory = listener_factory
        self._protos.append(proto)
        self._handlers.append(listener_factory)
        return proto.sockname()

    def stop_handler(self, handler):
        try:
            handler.proto.stop()
        except Exception as e:
            msg.warn('Error stopping connection: ', str_e(e))
        try:
            self._handlers.remove(handler)
        except Exception:
            pass
        try:
            self._protos.remove(handler.proto)
        except Exception:
            pass
        if hasattr(handler, 'listener_factory'):
            return handler.listener_factory.stop()
        if not self._handlers and not self._protos:
            msg.log('All handlers stopped. Stopping reactor.')
            self.stop()

    def stop(self):
        for _conn in self._protos:
            _conn.stop()

        self._protos = []
        self._handlers = []
        msg.log('Reactor shut down.')
        editor.status_message('Disconnected.')
        if self.on_stop:
            self.on_stop()

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

    @api.send_errors
    def tick(self, timeout=0):
        for factory in self._handlers:
            factory.tick()
        self.select(timeout)
        editor.call_timeouts()

    def block(self):
        while self._protos or self._handlers:
            self.tick(.05)

    def select(self, timeout=0):
        if not self._protos:
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
            fd_map[fileno] = fd

        if not readable and not writeable:
            return

        try:
            _in, _out, _except = select.select(readable, writeable, errorable, timeout)
        except (select.error, socket.error, Exception) as e:
            # TODO: with multiple FDs, must call select with just one until we find the error :(
            for fileno in readable:
                try:
                    select.select([fileno], [], [], 0)
                except (select.error, socket.error, Exception) as e:
                    fd_map[fileno].reconnect()
                    msg.error('Error in select(): ', fileno, str_e(e))
            return

        for fileno in _except:
            fd = fd_map[fileno]
            self._reconnect(fd, _in, _out)

        for fileno in _out:
            fd = fd_map[fileno]
            try:
                fd.write()
            except Exception as e:
                msg.error('Couldn\'t write to socket: ', str_e(e))
                msg.debug('Couldn\'t write to socket: ', pp_e(e))
                return self._reconnect(fd, _in)

        for fileno in _in:
            fd = fd_map[fileno]
            try:
                fd.read()
            except Exception as e:
                msg.error('Couldn\'t read from socket: ', str_e(e))
                msg.debug('Couldn\'t read from socket: ', pp_e(e))
                fd.reconnect()

reactor = _Reactor()
