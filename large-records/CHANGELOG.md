# Revision history for large-records

## 0.4.2 -- 2024-10-15

* Support `primitive-0.7.3` (#159, Isaac Elliott).
* Plugin idempotence (#159, Isaac Elliott).
* Document required additional dependencies and language extensions (#161).

Note: if your code imports `Data.Plugin.Record` only for the `largeRecord`
identifier used in the `ANN` annotations, this import is no longer required
as of this version and can be omitted.

## 0.4.1 -- 2024-05-30

* Support ghc 9.6 (and drop ghc <= 8.8)
  [together with Tristan Cacqueray and Gabriele Sales]

## 0.4 -- 2023-03-06

* Fix issue with operator type families used in fields (#120).
* Fix issue with `NamedWildCards` (#121, #124, #125).
* Do not generate imports in the plugin (#129).
  NOTE: This means that use code must now import `Data.Record.Plugin` to bring
  `largeRecord` into scope (necessary for `ANN` annotations).
* Support ghc 9.4 (#131).
  An annoying quirk of ghc 9.4 is that the order of plugins is reversed; this
  matters when using `large-records` and `record-dot-preprocessor` together.
  To avoid CPP, you can now use `Data.Record.Plugin.WithRDP`, which combines
  both plugins.
* Support `OverloadedRecordDot` and `OverloadedRecordUpdate` (#135).

## 0.3 -- 2022-07-22

* Support ghc 9.2 (#113 / #117).
* Support for field strictness annotations (#106 / #107).

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
