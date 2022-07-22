# Revision history for large-records

## 0.3.1 -- 2023-mm-dd

* Fix issue with operator type families used in fields (#120).
* Fix issue with `NamedWildCards` (#121, #124, #125).

## 0.3 -- 2022-07-22

* Support ghc 9.2 (#117)
* Support for field strictness annotations (#107)

## 0.2.1.0 -- 2022-04-06

* Update for `large-generics` 0.2

## 0.2.0.0 -- 2022-03-23

* Avoid all quotes: no more Template Haskell (#63) or quasi-quotes (#43).
  TH replaced by a source-plugin; quasi-quotes avoided by using a different
  internal representation, so that records can be constructed "as normal".
* Removed support for the pattern synonym, as it's not needed anymore.
* Compatible with ghc 8.10 and 9.0 (as well as 8.8).
* `large-generics` split off as a separate package (#45).
* Remove dependency on micro-lens (#27);
  `Data.Record.Lens.Micro` is now `Data.Record.Generic.Lens.VL`
  (and lives in `large-generics).
* Fix some strictness issues (#33).
* Refactored test suite and benchmarks.

## 0.1.0.0 -- 2021-08-19

* First public release.
