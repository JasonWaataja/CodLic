# Change Log
This project adheres to Semantic Versioning

## [0.1.4] - 2018-07-20
### Changed
- Completely change build system. It now actually works, adds dependency on
  `buildapp`.
- Make some error messages include file names.

## [0.1.3] - 2017-06-12
### Changed
- Change directory structure, now has a src dir.

### Fixed
- Fix bug where it would crash when trying to run regex on pathname object
  instead of string.
- Fix mixup of closing and contiunation comment strings in the C comment type so
  it would produce invalid syntax.

## [0.1.2] - 2017-06-12
### Added
- Add --print-languages and --print-licenses which print a list of useful items
  that can be used.

### Changed
- Change filetype regexes and comment types to use one internal table based on
  language name.

### Fixed
- Fix some errors in documentation.

### Added
- Add new search-replace feature for the license. This replaces the options
  --license-search and --license-replace from before. Now it can do multiple
  searches.

## [0.1.1] - 2017-06-09
### Fixed
- Fix numerous errors in README, including incorrect executable name and
  incorrect title.

## [0.1.0] - 2017-06-09
- Initial release.
