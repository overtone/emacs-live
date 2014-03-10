import os
import time

try:
    from . import shared as G
    assert G
    unicode = str
    python2 = False
except ImportError:
    python2 = True
    import shared as G


LOG_LEVELS = {
    'DEBUG': 1,
    'MSG': 2,
    'WARN': 3,
    'ERROR': 4,
}

LOG_LEVEL = LOG_LEVELS['MSG']
LOG_FILE = os.path.join(G.BASE_DIR, 'msgs.floobits.log')


try:
    fd = open(LOG_FILE, 'w')
    fd.close()
except Exception as e:
    pass


# Overridden by each editor
def editor_log(msg):
    print(msg)


class MSG(object):
    def __init__(self, msg, timestamp=None, username=None, level=LOG_LEVELS['MSG']):
        self.msg = msg
        self.timestamp = timestamp or time.time()
        self.username = username
        self.level = level

    def display(self):
        if self.level < LOG_LEVEL:
            return

        msg = unicode(self)
        if G.LOG_TO_CONSOLE or G.CHAT_VIEW is None:
            # TODO: ridiculously inefficient
            try:
                fd = open(LOG_FILE, 'a+')
                fd.write(msg)
                fd.close()
            except Exception as e:
                print(unicode(e))
            print(msg)
        else:
            editor_log(msg)

    def __str__(self):
        if python2:
            return self.__unicode__().encode('utf-8')
        return self.__unicode__()

    def __unicode__(self):
        if self.username:
            msg = '[{time}] <{user}> {msg}\n'
        else:
            msg = '[{time}] {msg}\n'
        return unicode(msg).format(user=self.username, time=time.ctime(self.timestamp), msg=self.msg)


def msg_format(message, *args, **kwargs):
    message += ' '.join([unicode(x) for x in args])
    if kwargs:
        message = unicode(message).format(**kwargs)
    return message


def _log(message, level, *args, **kwargs):
    if level >= LOG_LEVEL:
        MSG(msg_format(message, *args, **kwargs), level=level).display()


def debug(message, *args, **kwargs):
    _log(message, LOG_LEVELS['DEBUG'], *args, **kwargs)


def log(message, *args, **kwargs):
    _log(message, LOG_LEVELS['MSG'], *args, **kwargs)


def warn(message, *args, **kwargs):
    _log(message, LOG_LEVELS['WARN'], *args, **kwargs)


def error(message, *args, **kwargs):
    _log(message, LOG_LEVELS['ERROR'], *args, **kwargs)
