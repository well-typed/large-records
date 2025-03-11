# Revision history for large-anon

## 0.3.2 -- 2025-03-11

* Use ghc-tcplugin-api 0.14 [Chan Siu Man]

## 0.3.1 -- 2024-05-30

* Support ghc 9.6 (and drop ghc <= 8.8)
  [together with Gabriele Sales]

## 0.3 -- 2023-07-04

* Critical bugfix (#146): incremental record construction was fundamentally
  broken. This is a major version dump because the internal representation
  is different, so if any code happens to rely on that, it will need to be
  updated. Specifically, indices into the underlying representation of the
  record are now interpreted from the _end_ of the array, rather than the
  start; this is necessary to ensure that inserting new fields (which are
  prepended to the record row) do not affect the indices of old fields.
  With thanks to Johannes Gerer for the bug report and initial investigation.
* One somewhat unfortunate consequence of this new design is that we can no
  longer resolve constraints `HasField` constraints unless _all_ fields in the
  record are known. Previously, we had limited support for resolving `HasField`
  constraints for rows such as `(a := Int ': r)`. However, since this
  support did not extend to other constraints, it was probably not very useful
  anyway; `large-anon` is quite explicit about not supporting inductive
  reasoning.
* Add `disableFourmoluExec` flag to disable the Fourmolu tests in the test suite
  (#145; together with Johannes Gerer).

## 0.2.1 -- 2023-06-05

* Add `distribute` (Johannes Gerer, #142)

## 0.2 -- 2023-03-06

* Do not generate imports in the plugin (#129).
* Support ghc 9.4 (#131).
* Use `ANON`/`ANON_F` in `Show` instance (#103).
* Support `OverloadedRecordDot` and `OverloadedRecordUpdate` (#128).

## 0.1.1 -- 2022-07-22

* Support for ghc 9.2 (#116)

## 0.1.0.0 -- 2022-04-06

* First public release
