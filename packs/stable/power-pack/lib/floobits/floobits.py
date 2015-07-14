#!/usr/bin/env python
# coding: utf-8
import os
import sys
import optparse
from floo import emacs_handler
from floo.common import migrations
from floo.common import reactor
from floo.common import utils
from floo.common import shared as G


def cb(port):
    print('Now listening on %s' % port)
    sys.stdout.flush()


def main():
    G.__VERSION__ = '0.11'
    G.__PLUGIN_VERSION__ = None

    parser = optparse.OptionParser(usage='%prog [options]')
    parser.add_option("-p", "--port",
                      dest="port",
                      default=0,
                      help="The port to listen on. Useful for debugging.")
    parser.add_option("--set-version",
                      dest="version")

    options, args = parser.parse_args()
    port = int(options.port)
    G.__PLUGIN_VERSION__ = options.version
    if not G.__PLUGIN_VERSION__:
        print('--set-version is a required argument')
        print('args: %s' % ' '.join(args))
        sys.exit(1)

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
    _, port = reactor.reactor.listen(emacs, port=port)
    utils.set_timeout(cb, 100, port)
    reactor.reactor.block()

if __name__ == '__main__':
    main()
