# Revision history for typed-protocols-cborg

## [Unreleased]

- A major redesign of `typed-protocols`.  `Protocol` class requires data family
  `Message` and new associated type familiy instance `StateAgency`.  One also
  needs to define singletons and `Sing` & `SingI` instances from the
  [`singletons`][singletons-3.0.1] package.

## 0.1.0.5 -- 2023-03-08

* Support `ghc-9.6.1`.
* Use `io-classes-1.1.0.0`.

[singletons-3.0.1]: https://hackage.haskell.org/package/singletons 

