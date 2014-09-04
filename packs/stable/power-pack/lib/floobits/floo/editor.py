import sys
import os
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


NEW_ACCOUNT_TXT = 'Welcome {username}!\n\nYou\'re all set to collaborate. You should check out our docs at https://{host}/help/plugins/emacs#usage. \
You must run \'Floobits - Complete Sign Up\' so you can log in to the website.'

LINKED_ACCOUNT_TXT = """Welcome {username}!\n\nYou are all set to collaborate.

You may want to check out our docs at https://{host}/help/plugins/emacs#usage"""

line_endings = os.linesep


def name():
    if sys.version_info < (3, 0):
        py_version = 2
    else:
        py_version = 3
    return 'Emacs-py%s' % py_version


def codename():
    return 'emacs'


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
    for t, tos in timeouts.copy().items():
        if now >= t:
            for timeout in tos:
                timeout()
            to_remove.append(t)
    for k in to_remove:
        del timeouts[k]
    calling_timeouts = False


def error_message(msg):
    emacs = getattr(G, 'emacs', None)
    if emacs:
        emacs.error_message(msg)
    else:
        print(msg)


def status_message(msg):
    emacs = getattr(G, 'emacs', None)
    if emacs:
        emacs.status_message(msg)
    else:
        print(msg)


def message_dialog(msg):
    # TODO: make this a modal thing. probably need to open a new buffer
    emacs = getattr(G, 'emacs', None)
    if emacs:
        emacs.status_message(msg)
    else:
        print(msg)


def open_file(f):
    emacs = getattr(G, 'emacs', None)
    if not emacs:
        return
    event = {
        'name': 'open_file',
        'filename': f
    }
    emacs.send(event)


def platform():
    return sys.platform


def get_line_endings(path=None):
    return line_endings
