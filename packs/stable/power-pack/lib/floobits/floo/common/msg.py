import os
import time

try:
    from . import shared as G
    assert G
    unicode = str
    from .exc_fmt import str_e
    python2 = False
except ImportError:
    python2 = True
    from exc_fmt import str_e
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


def safe_print(msg):
    # Some environments can have trouble printing unicode:
    #    "When print() is not outputting to the terminal (being redirected to
    #    a file, for instance), print() decides that it does not know what
    #    locale to use for that file and so it tries to convert to ASCII instead."
    # See: https://pythonhosted.org/kitchen/unicode-frustrations.html#frustration-3-inconsistent-treatment-of-output
    try:
        print(msg)
    except UnicodeEncodeError:
        print(msg.encode('utf-8'))


# Overridden by each editor
def editor_log(msg):
    safe_print(msg)


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
                fd = open(LOG_FILE, 'ab')
                fmsg = msg
                try:
                    fmsg = fmsg.encode('utf-8')
                except Exception:
                    pass
                fd.write(fmsg)
                fd.write(b'\n')
                fd.close()
            except Exception as e:
                safe_print(str_e(e))
            safe_print(msg)
        else:
            editor_log(msg)

    def __str__(self):
        if python2:
            return self.__unicode__().encode('utf-8')
        return self.__unicode__()

    def __unicode__(self):
        if self.username:
            msg = '[{time}] <{user}> {msg}'
        else:
            msg = '[{time}] {msg}'
        try:
            return unicode(msg).format(user=self.username, time=time.ctime(self.timestamp), msg=self.msg)
        except UnicodeEncodeError:
            return unicode(msg).format(user=self.username, time=time.ctime(self.timestamp), msg=self.msg.encode(
                'utf-8'))


def msg_format(message, *args, **kwargs):
    try:
        message = unicode(message)
    except UnicodeEncodeError:
        message = str(message)
    for arg in args:
        try:
            message += unicode(arg)
        except UnicodeEncodeError:
            message += arg
    if kwargs:
        message = message.format(**kwargs)
    return message


def _log(message, level, *args, **kwargs):
    if level >= LOG_LEVEL:
        # TODO: kill MSG class and just format and print the thing right away
        MSG(msg_format(message, *args, **kwargs), level=level).display()


def debug(message, *args, **kwargs):
    _log(message, LOG_LEVELS['DEBUG'], *args, **kwargs)


def log(message, *args, **kwargs):
    _log(message, LOG_LEVELS['MSG'], *args, **kwargs)


def warn(message, *args, **kwargs):
    _log(message, LOG_LEVELS['WARN'], *args, **kwargs)


def error(message, *args, **kwargs):
    _log(message, LOG_LEVELS['ERROR'], *args, **kwargs)
