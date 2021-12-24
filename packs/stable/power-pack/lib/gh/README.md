[![Build Status](https://travis-ci.org/sigma/gh.el.svg?branch=master)](https://travis-ci.org/sigma/gh.el)
[![Coverage Status](https://coveralls.io/repos/github/sigma/gh.el/badge.svg?branch=master)](https://coveralls.io/github/sigma/gh.el?branch=master)

This is a GitHub client library for Emacs.

This library also allows implementation of the various authentication schemes (password, OAuth).

Implementation is heavily based on EIEIO so that various components can be replaced easily.

Current state:
* API v3 handler

* functional password- and oauth-based (default) authentication

* low-level APIs
  * auth
  * comments
  * gists (used in separate project http://github.com/defunkt/gist.el )
  * issues
  * orgs
  * pull requests (used in separate project http://github.com/sigma/magit-gh-pulls )
  * repositories
  * search
  * users

* support for local caching
