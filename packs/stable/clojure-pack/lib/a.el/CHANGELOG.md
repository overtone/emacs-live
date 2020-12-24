# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]
(this space intentionally left blank)

<!-- ### Changed -->
<!-- ### Added -->
<!-- ### Changed -->
<!-- ### Removed -->
<!-- ### Fixed -->

## [0.1.1] - 2018-09-07
### Fixed

- Fixed version string in source file

## [0.1.0] - 2018-08-06
### Added

- Added `a-get*`

## [0.1.0alpha4] - 2017-07-20
### Fixed
- Fixed `a-get`, `a-has-key`, and `a-get-in` to correctly work with string keys.

## [0.1.0alpha3] - 2017-07-16
### Added
- `a-dissoc` Remove a key from a collection
- `a-associative?` / `a-associative-p` Returns true for association lists and hash tables
- `a-merge-with` Merge, resolving conflicts with a given function

### Changed
- `a-equal` now works on any value, and does a deep comparison
- `a-equal` terminates early
- Removed dash dependency

## [0.1.0alpha2] - 2017-07-15
### Added
- `a-hash-table` Hash table constructor, similar to `a-list`

[Unreleased]: https://github.com/plexus/a.el/compare/v0.1.0alpha3...HEAD
[0.1.0alpha4]: https://github.com/plexus/a.el/compare/v0.1.0alpha3...v0.1.0alpha4
[0.1.0alpha3]: https://github.com/plexus/a.el/compare/v0.1.0alpha2...v0.1.0alpha3
[0.1.0alpha2]: https://github.com/plexus/a.el/compare/6760b4edb7cf...v0.1.0alpha2
