# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project does not currently adhere to a particular versioning scheme.

## [Unreleased]

### Fixed

- Fixed readback of numeric operations. ([#467][gh-467])

## [0.2.35] - 2024-06-06

### Changed

- Make lambda `Term` with bind patterns display as `let` terms. ([#466][gh-466])

## [0.2.34] - 2024-06-05

### Added

- Create `<=` and `>=` operators. ([#451][gh-451])

## [0.2.33] - 2024-06-05

### Added

- Implement `expand_main`, a compilation pass that expands references in the entry point function. ([#424][gh-424])

### Changed

- Make the `float_combinators` pass no longer extract in the entry point function. ([#424][gh-424])

## [0.2.32] - 2024-06-05

### Added

- Implement the built-in `Tree` datatype. ([#528][gh-528])
- Implement `![]` and `!` syntax for `Tree` literals. ([#528][gh-528])
- Create a changelog.

### Changed

- Move the builtins documentation to `/docs`.

## [0.2.30] - 2024-06-04

### Changed

- No longer expand generated recursive definitions. ([#502][gh-502])

## [0.2.29] - 2024-06-04

### Added

- Support custom HVM binaries. ([#479][gh-479])

### Changed

- Make monadic blocks lazy by deferring execution of continuations with free vars. ([#526][gh-526])

## [0.2.28] - 2024-05-30

### Added

- Support mapper statements. ([#465][gh-465])

## [0.2.27] - 2024-05-29

### Changed

- Make `with` clauses take a bind and an argument

## [0.2.0] - 2024-05-14

- Initial public release of Bend.

[0.2.0]: https://github.com/HigherOrderCO/Bend/releases/tag/0.2.0
[0.2.27]: https://github.com/HigherOrderCO/Bend/releases/tag/0.2.27
[0.2.28]: https://github.com/HigherOrderCO/Bend/releases/tag/0.2.28
[0.2.29]: https://github.com/HigherOrderCO/Bend/releases/tag/0.2.29
[0.2.30]: https://github.com/HigherOrderCO/Bend/releases/tag/0.2.30
[0.2.32]: https://github.com/HigherOrderCO/Bend/releases/tag/0.2.32
[0.2.33]: https://github.com/HigherOrderCO/Bend/releases/tag/0.2.33
[0.2.34]: https://github.com/HigherOrderCO/Bend/releases/tag/0.2.34
[0.2.35]: https://github.com/HigherOrderCO/Bend/releases/tag/0.2.35
[gh-424]: https://github.com/HigherOrderCO/Bend/issues/424
[gh-451]: https://github.com/HigherOrderCO/Bend/issues/451
[gh-465]: https://github.com/HigherOrderCO/Bend/issues/465
[gh-466]: https://github.com/HigherOrderCO/Bend/issues/466
[gh-467]: https://github.com/HigherOrderCO/Bend/issues/467
[gh-479]: https://github.com/HigherOrderCO/Bend/issues/479
[gh-502]: https://github.com/HigherOrderCO/Bend/issues/502
[gh-526]: https://github.com/HigherOrderCO/Bend/issues/526
[gh-528]: https://github.com/HigherOrderCO/Bend/issues/528
[Unreleased]: https://github.com/HigherOrderCO/Bend/compare/0.2.35...HEAD
