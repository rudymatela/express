Haexpress
=========

[![Haexpress's Build Status][build-status]][build-log]
[![Haexpress on Hackage][hackage-version]][haexpress-on-hackage]
[![Haexpress on Stackage LTS][stackage-lts-badge]][haexpress-on-stackage-lts]
[![Haexpress on Stackage Nightly][stackage-nightly-badge]][haexpress-on-stackage-nightly]

![Haexpress logo][haexpress-logo]


Haexpress is a library for manipulating dynamically typed Haskell expressions.
It's like `Data.Dynamic` but with support for:

* encoding applications;
* variables;
* string representation.

[`Expr`] is the type we use to encode expressions.


Example: heterogeneous lists
----------------------------

Like with `Data.Dynamic`, we can use Haexpress to create heterogeneous lists.

For types that are [`Show`] instances, we can use [`val`] to encode values as [`Expr`]s.
Here, we use applications of [`val`] to create a heterogeneous list:

```
> let xs = [val False, val True, val (1::Int), val (2::Int), val (3::Integer), val "123"]
> :t xs
xs :: [Expr]
> xs
[False :: Bool,True :: Bool,1 :: Int,2 :: Int,3 :: Integer,"123" :: [Char]]
```

We can then apply [`evaluate`] to select values of different types:

```
> import Data.Maybe
> mapMaybe evaluate xs :: [Bool]
[False,True]
> mapMaybe evaluate xs :: [Int]
[1,2]
> mapMaybe evaluate xs :: [Integer]
[3]
> mapMaybe evaluate xs :: [String]
["123"]
```


Installing
----------

To install the latest Haexpress version from Hackage, just run:

	$ cabal update
	$ cabal install haexpress


[Haexpress's Haddock documentation]: https://hackage.haskell.org/package/haexpress/docs/Data-Haexpress.html

[haexpress-logo]: https://github.com/rudymatela/haexpress/raw/master/doc/haexpress.svg?sanitize=true

[build-status]: https://travis-ci.org/rudymatela/haexpress.svg?branch=master
[build-log]:    https://travis-ci.org/rudymatela/haexpress
[hackage-version]: https://img.shields.io/hackage/v/haexpress.svg
[haexpress-on-hackage]: https://hackage.haskell.org/package/haexpress
[stackage-lts-badge]:            https://stackage.org/package/haexpress/badge/lts
[stackage-nightly-badge]:        https://stackage.org/package/haexpress/badge/nightly
[haexpress-on-stackage]:         https://stackage.org/package/haexpress
[haexpress-on-stackage-lts]:     https://stackage.org/lts/package/haexpress
[haexpress-on-stackage-nightly]: https://stackage.org/nightly/package/haexpress
