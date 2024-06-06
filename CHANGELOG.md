# Changelog

## 0.2.35
- Changed lambda `Term` with bind patterns to display as `let` terms.

## 0.2.34
- Added `<=` and `>=` operators. (#451)

## 0.2.33
- Added `expand_main`, a compilation pass that expands references in the entry point function. (#424)
- Changed the `float_combinators` pass to not extract in the entry point function. (#424)

## 0.2.32
- Added the built-in `Tree` datatype. (#528)
- Added `![]` and `!` syntax for `Tree` literals. (#528)
- Moved the builtins documentation to `/docs`.
- Created the changelog.

## 0.2.0
- Initial public release of Bend.
