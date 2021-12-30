# 1.0.6 (2021-10-13)

- Make sure parseedn and parseclj versions are in sync

# 1.0.5 (2021-10-13)

- [#34](https://github.com/clojure-emacs/parseclj/pull/34) Replace `cl-case` with `cond`

# 1.0.4 (2021-09-30)

- Provide parseclj-alist-merge, since we can't use `(map-merge 'alist)` yet in Emacs 25/26.

# 1.0.3 (2021-09-29)

- Remove remaining a.el usage (this time for real)

# 1.0.1 (2021-09-27)

- Remove remaining a.el usage

# 1.0.0 (2021-09-27)

- Added a `:read-one` option to read/parse a single form at a time
- Support more reader dispatch macro forms: eval (`#=`), shebang (`#!`),
  symbolic value (`##NaN`)

# 0.2.0 (2020-10-12)

- raise error on unmatched closing paren/brace

# 0.1.0 (2018-05-27)

Initial release.
