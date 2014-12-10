#!/usr/bin/env python
# coding: utf-8
import os

from floo import emacs_handler
from floo.common import migrations
from floo.common import reactor
from floo.common import utils
from floo.common import shared as G


def cb(port):
    print('Now listening on %s' % port)


def main():
    G.__VERSION__ = '0.11'
    G.__PLUGIN_VERSION__ = '1.5.13'
    utils.reload_settings()

    if not os.path.exists(G.FLOORC_JSON_PATH):
        migrations.migrate_floorc()
        utils.reload_settings()

    migrations.rename_floobits_dir()
    migrations.migrate_symlinks()

    try:
        utils.normalize_persistent_data()
    except Exception:
        pass

    emacs = emacs_handler.EmacsHandler()
    G.emacs = emacs
    _, port = reactor.reactor.listen(emacs)
    utils.set_timeout(cb, 100, port)
    reactor.reactor.block()

if __name__ == '__main__':
    main()
