<!--
Added for new features.
Changed for changes in existing functionality.
Deprecated for soon-to-be removed features.
Removed for now removed features.
Fixed for any bug fixes.
Security in case of vulnerabilities.
-->

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic
Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.3.1] - 2026-04-26

### Added

- ([#22](https://github.com/gamma0987/either-or-both/pull/22)): Implement
  `Ord` and `PartialOrd` for `Either` and `EitherOrBoth`.
- ([#24](https://github.com/gamma0987/either-or-both/pull/24)): Add more
  `#[must_use]` annotations.
- ([#26](https://github.com/gamma0987/either-or-both/pull/26)): Implement
  `Error` for `EitherOrBoth`.

### Changed

- ([#28](https://github.com/gamma0987/either-or-both/pull/28)): Sort variants of
  the `EitherOrBoth` enum like the `Ord` implementation: `Left`, `Both`, `Right`
  instead of `Both`, `Left`, `Right`.

### Fixed

- ([#23](https://github.com/gamma0987/either-or-both/pull/23)): Fix example
  using `EitherOrBoth` instead of `Either`. Fix some unbalanced backticks in
  error messages.
- ([#27](https://github.com/gamma0987/either-or-both/pull/27)):
  `EitherOrBoth::map_right` now uses a `where` clause for the function `F` like
  `map_left`.

## [0.3.0] - 2025-08-30

### Added

- ([#12](https://github.com/gamma0987/either-or-both/pull/12)): New
  methods `EitherOrBoth::both_and`, `EitherOrBoth::both_and_then`.
- ([#12](https://github.com/gamma0987/either-or-both/pull/12)): FromIterator
  implementations of iterators over `Either` or `EitherOrBoth` into
  `EitherOrBoths` of `IndexMap`, `IndexSet`, `HashMap`, `HashSet`.

### Changed

- ([#12](https://github.com/gamma0987/either-or-both/pull/12)): Add
  `#[must_use]` annotations where appropriate.
- ([#12](https://github.com/gamma0987/either-or-both/pull/12)): Use `const` on
  methods that allow it.
- ([#12](https://github.com/gamma0987/either-or-both/pull/12)): Add
  `#[track_caller]` on all `unwrap`- and `extend`-like methods
- ([#12](https://github.com/gamma0987/either-or-both/pull/12)): Add `#[inline]`
  for most methods.

## [0.2.0] - 2025-08-28

### Added

- ([#8](https://github.com/gamma0987/either-or-both/pull/8)): Derive
  `schemars::JsonSchema` if the schema feature is activated.

### Fixed

- ([#8](https://github.com/gamma0987/either-or-both/pull/8)): Fix the feature
  usage of serde in `no_std` environments.

## [0.1.0] - 2025-08-27

### Added

- Initial release
