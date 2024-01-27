# Changelog

## [0.5.0] - 2023-05-13

### Changed

- Tidy up and organise the nix files.
- **BREAKING:** Change mkElmDerivation's inputs slightly to allow for a more general derivation. See the wiki for more information.
- Improve `elm-hasher` source structure.

### Added

- Add =elm-watch= support.

### Removed

- **BREAKING:** Remove `${system}` from the overlays. This breaking change simplifies the overlay usage, and is arguably more correct than the previous approach.


## [0.4.0] - 2023-03-31

### Added

- Add `mkElmSpaDerivation` function for building elm-spa apps;
- Add ability to pass arguments to the mkDerivation call;
- Add elmHashes derivation, exposing the elm-hashes.json for public use.


## [0.3.0] - 2023-03-17

### Removed

- Remove the binary `registry.dat` from version control. It is now generated when building the elm package.


## [0.2.0.0] - 2023-03-06

### Changed

- Refactor code into both an overlay and a flake;
- **BREAKING:** Remove "nixpkgs" dependency in mkElmDerivation;
- Run `elm make` with `--optimize` flag.


## [0.1.1.0] - 2023-01-30

_Add improvements to memory and speed of the hasher application._

### Changed

- Refactor code to increase readability and modularity;
- Half the heap memory usage;
- Change hash function to one from Cryptonite.

### Added

- Add report at the end of the programme.

### Fixed

- Fix occasional deadlock with MVars.


## [0.1.0.0] - 2023-01-15

_Initial release._

[0.5.0]: https://github.com/jeslie0/mkElmDerivation/releases/tag/v0.5.0
[0.4.0]: https://github.com/jeslie0/mkElmDerivation/releases/tag/v0.4.0
[0.3.0]: https://github.com/jeslie0/mkElmDerivation/releases/tag/v0.3.0
[0.2.0.0]: https://github.com/jeslie0/mkElmDerivation/releases/tag/v0.2.0.0
[0.1.1.0]: https://github.com/jeslie0/mkElmDerivation/releases/tag/v0.1.1.0
[0.1.0.0]: https://github.com/jeslie0/mkElmDerivation/releases/tag/v0.1.0.0
