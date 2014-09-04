try:
    from .. import event_emitter
except (ImportError, ValueError):
    from floo.common import event_emitter


class BaseProtocol(event_emitter.EventEmitter):
    ''' Base FD Interface'''

    def __init__(self, host, port, secure=True):
        super(BaseProtocol, self).__init__()
        self.host = host
        self.port = port
        self.secure = secure

    def __len__(self):
        return 0

    def fileno(self):
        raise NotImplementedError("fileno not implemented.")

    def fd_set(self, readable, writeable, errorable):
        raise NotImplementedError("fd_set not implemented.")

    def cleanup(self):
        raise NotImplementedError("clean up not implemented.")

    def write(self):
        raise NotImplementedError("write not implemented.")

    def read(self):
        raise NotImplementedError("read not implemented.")

    def error(self):
        raise NotImplementedError("error not implemented.")

    def reconnect(self):
        raise NotImplementedError("reconnect not implemented.")

    def reset_retries(self):
        raise NotImplementedError("reset_retries not implemented.")

    def stop(self):
        self.cleanup()

    def connect(self, conn=None):
        self.emit("connect", conn)
