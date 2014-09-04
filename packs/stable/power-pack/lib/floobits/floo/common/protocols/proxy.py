import subprocess
import re
import os.path

try:
    import fcntl
except Exception:
    pass

try:
    from .. import event_emitter, msg, shared as G
    assert event_emitter and G and msg
except (ImportError, ValueError):
    from floo.common import event_emitter, msg, shared as G


class ProxyProtocol(event_emitter.EventEmitter):
    ''' Base Proxy Interface'''

    def __init__(self):
        super(ProxyProtocol, self).__init__()
        try:
            from .. import reactor
        except (ImportError, ValueError):
            from floo.common import reactor
        self.reactor = reactor.reactor
        self.cleanup()

    def __len__(self):
        return 0

    def fileno(self):
        return self.fd

    def fd_set(self, readable, writeable, errorable):
        if self.fd:
            readable.append(self.fd)
            errorable.append(self.fd)

    def cleanup(self):
        try:
            self._proc.kill()
        except Exception:
            pass
        self.fd = None
        self._proc = None
        self.buf = [b'']

    def read(self):
        if self.fd is None:
            msg.debug('self.fd is None. Read called after cleanup.')
            return
        data = b''
        while True:
            try:
                d = os.read(self.fd, 65535)
                if not d:
                    break
                data += d
            except (IOError, OSError):
                break
        self.buf[0] += data
        if not data:
            return
        while True:
            before, sep, after = self.buf[0].partition(b'\n')
            if not sep:
                break
            self.buf[0] = after
            try:
                msg.debug('Floobits SSL proxy output: ', before.decode('utf-8', 'ignore'))
            except Exception:
                pass

    def error(self):
        self.cleanup()

    def reconnect(self):
        self.stop()

    def stop(self):
        self.cleanup()
        self.emit('stop')

    def connect(self, args):
        msg.debug('Running proxy with args ', args, ' in ', G.PLUGIN_PATH)
        self._proc = proc = subprocess.Popen(args, cwd=G.PLUGIN_PATH, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        line = proc.stdout.readline().decode('utf-8')
        self.fd = proc.stdout.fileno()
        fl = fcntl.fcntl(self.fd, fcntl.F_GETFL)
        fcntl.fcntl(self.fd, fcntl.F_SETFL, fl | os.O_NONBLOCK | os.O_ASYNC)

        msg.log('Read line from Floobits SSL proxy: ', line)
        match = re.search('Now listening on <(\d+)>', line)
        if not match:
            raise Exception("Couldn't find port in line from proxy: %s" % line)
        self._port = int(match.group(1))
        self.reactor._protos.append(self)
        return self._port
