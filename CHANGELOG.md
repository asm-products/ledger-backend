# Changelog

This package uses [Semantic Versioning][1].

## v0.3.1 (2014-12-25)

- Fixed a bug that incorrectly calculated the maximum entry ID.
- Fixed a bug that prevented looking up some entries by ID.

## v0.3.0 (2014-12-14)

- Changed how entries are stored by only storing the key ID instead of the
  entire key. Did not provide a migration.

## v0.2.3 (2014-12-13)

- Removed Unicode from startup message.

## v0.2.2 (2014-12-13)

- Added a message when the server starts.

## v0.2.1 (2014-12-11)

- Added support for GHC 7.6.

## v0.2.0 (2014-12-10)

- Rewrote everything. Don't assume that anything still works the same.

## v0.1.5 (2014-11-24)

- Added the ability to search and filter entries.

## v0.1.4 (2014-11-21)

- Switch to Bootstrap.

## v0.1.3 (2014-11-09)

- Styled the interface.

## v0.1.2 (2014-11-06)

- Renamed "acid" configuration section to "acid-state".
- Added the ability to authenticate API requests using a key.
- Added the ability to edit entries from the UI.
- Moved JSON endpoints under `api/`.
- Removed HTTP basic authentication.
- Removed socket support for acid-state.

## v0.1.1 (2014-11-02)

- Added HTTP basic authentication.
- Added the ability use local or remote state.

## v0.1.0 (2014-11-01)

- Initially released.

## v0.0.0 (2014-10-29)

- Initially committed.

[1]: http://semver.org/spec/v2.0.0.html
