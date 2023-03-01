# Scalable records in Haskell

This repository contains all of the work on scalable records in Haskell:

- [large-records](large-records/) provides scalable _named_ records (records that are declared before they are used, just like standard Haskell records).
- [large-anon](large-anon/) provides scalable _anonymous_ records.
- [large-generics](large-generics/) is the generics library that supports both
  `large-records` and `large-anon`.
- [beam-large-records](beam-large-records/) provides integration for
  `large-records` with [beam](https://hackage.haskell.org/package/beam-core).
- [typelet](typelet/) provides experimental support for type-level sharing.
- [large-records-benchmarks](large-records-benchmarks) contains a large number
  of benchmarks measuring the (compile time) performance of the `large-records`
  library.

For a detailed overview on compile time performance of `large-records`, see
the [benchmarks report](large-records-benchmarks/report/).

The problems addressed by these libraries are discussed in a series of
blog posts:

- [Avoiding quadratic core code size with large records](https://well-typed.com/blog/2021/08/large-records/)
- [Induction without core-size blow-up a.k.a. Large records: anonymous edition](https://well-typed.com/blog/2021/10/large-records-part-2/)
- [Type-level sharing in Haskell, now](https://well-typed.com/blog/2021/12/type-level-sharing-now/)
- [New large-records release: now with 100% fewer quotes](https://well-typed.com/blog/2022/03/large-records-without-the-quotes/)
- [`large-anon`: Practical scalable anonymous records for Haskell](https://well-typed.com/blog/2022/04/large-anon/)

as well as in various presentations:

- [Avoiding quadratic GHC core code size: Introducing the large-records library](https://www.youtube.com/watch?v=XXPWVPquYvw), Haskell Implementors’ Workshop 2021
- [Avoid quadratic blow-up during compilation](https://skillsmatter.com/skillscasts/17262-avoiding-quadratic-blow-up-during-compilation), Haskell Exchange 2021



