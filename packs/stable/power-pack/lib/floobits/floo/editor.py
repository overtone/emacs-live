import sys
from collections import defaultdict
import time

try:
    from .common import shared as G
except (ImportError, ValueError):
    import common.shared as G


timeouts = defaultdict(list)
top_timeout_id = 0
cancelled_timeouts = set()
calling_timeouts = False
welcome_text = ''


def name():
    if sys.version_info < (3, 0):
        py_version = 2
    else:
        py_version = 3
    return 'Emacs-py%s' % py_version


def windows(*args, **kwargs):
    return []


def set_timeout(func, timeout, *args, **kwargs):
    global top_timeout_id
    timeout_id = top_timeout_id
    top_timeout_id + 1
    if top_timeout_id > 100000:
        top_timeout_id = 0

    def timeout_func():
        if timeout_id in cancelled_timeouts:
            cancelled_timeouts.remove(timeout_id)
            return
        func(*args, **kwargs)

    then = time.time() + (timeout / 1000.0)
    timeouts[then].append(timeout_func)
    return timeout_id


def cancel_timeout(timeout_id):
    if timeout_id in timeouts:
        cancelled_timeouts.add(timeout_id)


def call_timeouts():
    global calling_timeouts
    if calling_timeouts:
        return
    calling_timeouts = True
    now = time.time()
    to_remove = []
    for t, tos in timeouts.items():
        if now >= t:
            for timeout in tos:
                timeout()
            to_remove.append(t)
    for k in to_remove:
        del timeouts[k]
    calling_timeouts = False


def error_message(*args, **kwargs):
    editor = getattr(G, 'editor', None)
    if editor:
        editor.error_message(*args, **kwargs)
    else:
        print(args, kwargs)


def status_message(msg):
    editor = getattr(G, 'editor', None)
    if editor:
        editor.status_message(msg)
    else:
        print(msg)


def message_dialog(msg):
    editor = getattr(G, 'editor', None)
    if editor:
        editor.status_message(msg)
    else:
        print(msg)


def open_file(file):
    raise NotImplementedError('open_file')


def platform():
    return sys.platform
