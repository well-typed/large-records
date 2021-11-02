# Benchmarks for the HaskellX talk

Note on ghc 9.2: ghc 9.2 introduces `NoFieldSelectors`, but it does not have
any affect on the "simple record" case, because the record selectors are still
_generated_, they're just not exported as user-visible names. It's also a bit
awkward to measure, since `ghc-dump` is not yet compatible with 9.2.
