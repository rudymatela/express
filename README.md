Haexpress
=========

[![Haexpress's Build Status][build-status]][build-log]
[![Haexpress on Hackage][hackage-version]][haexpress-on-hackage]
[![Haexpress on Stackage LTS][stackage-lts-badge]][haexpress-on-stackage-lts]
[![Haexpress on Stackage Nightly][stackage-nightly-badge]][haexpress-on-stackage-nightly]

![Haexpress logo][haexpress-logo]


Haexpress is a library for manipulating dynamically typed Haskell expressions.
It's like [`Data.Dynamic`] but with support for:

* encoding applications;
* variables;
* string representation.

[`Expr`] is the type we use to encode expressions.


Installing
----------

To install the latest Haexpress version from Hackage, just run:

	$ cabal update
	$ cabal install haexpress


Example 1: heterogeneous lists
------------------------------

Like with [`Data.Dynamic`], we can use Haexpress to create heterogeneous lists.

For types that are [`Show`] instances, we can use [`val`] to encode values as [`Expr`]s.
Here, we use applications of [`val`] to create a heterogeneous list:

	> let xs = [val False, val True, val (1::Int), val (2::Int), val (3::Integer), val "123"]
	> :t xs
	xs :: [Expr]
	> xs
	[ False :: Bool
	, True :: Bool
	, 1 :: Int
	, 2 :: Int
	, 3 :: Integer
	, "123" :: [Char]
	]

We can then apply [`evaluate`] to select values of different types:

	> import Data.Maybe
	> mapMaybe evaluate xs :: [Bool]
	[False,True]
	> mapMaybe evaluate xs :: [Int]
	[1,2]
	> mapMaybe evaluate xs :: [Integer]
	[3]
	> mapMaybe evaluate xs :: [String]
	["123"]


Example 2: listing applications
-------------------------------

Carrying on from Example 1, we define an heterogeneous list of functions
encoded as [`Expr`]s:

	> let fs = [value "not" not, value "&&" (&&), value "abs" (abs :: Int -> Int)]
	> :t fs
	fs :: [Expr]

Using [`$$`] we list the type correct applications of functions in `fs` to
values in `xs`.

	> import Data.Maybe (catMaybes)
	> catMaybes [f $$ x | f <- fs, x <- xs]
	[ not False :: Bool
	, not True :: Bool
	, (False &&) :: Bool -> Bool
	, (True &&) :: Bool -> Bool
	, abs 1 :: Int
	, abs 2 :: Int
	]

Example 3: u-Extrapolate
------------------------

This example shows how to build a property-based testing library capable of
generalizing counter-examples in under 40 lines of code.  Besides, using
Haexpress to encode expressions, it uses [LeanCheck] for generating test
values.

	import Data.Haexpress
	import Test.LeanCheck hiding (counterExample, check)

Given a maximum number of tests and a property, the following `counterExample`
function returns either `Nothing` when tests pass or `Just` a counterexample
encoded as an [`Expr`].

	counterExample :: (Listable a, Express a) => Int -> (a -> Bool) -> Maybe Expr
	counterExample maxTests prop  =  listToMaybe
	  [expr x | x <- take maxTests list, not (prop x)]

Examples (REPL):

	> counterExample 100 (\(x,y) -> x + y == y + x)
	Nothing
	> counterExample 100 (\x -> x == x + x)
	Just (1 :: Integer)
	> counterExample 100 (\xs -> nub xs == (xs :: [Int]))
	Just ([0,0] :: [Int])

Before moving on to generalize counterexamples, we need a way to compute ground
expressions from an expression with variables.  For that, we will use `grounds`
and `tiersFor`:

	grounds :: Expr -> [Expr]
	grounds e  =  map (e //-)
	           .  concat
	           $  products [mapT ((,) v) (tiersFor v) | v <- nubVars e]

	tiersFor :: Expr -> [[Expr]]
	tiersFor e  =  case show (typ e) of
	  "Int"    ->  mapT val (tiers `asTypeOf` [[undefined :: Int]])
	  "Bool"   ->  mapT val (tiers `asTypeOf` [[undefined :: Bool]])
	  "[Int]"  ->  mapT val (tiers `asTypeOf` [[undefined :: [Int]]])
	  "[Bool]" ->  mapT val (tiers `asTypeOf` [[undefined :: [Bool]]])
	  _        ->  []

Above, we restrict ourselves to `Int`, `Bool`, `[Int]` and `[Bool]` as test
types.  So we can now compute the grounds of an expression with variables:

	> grounds (value "not" not :$ var "p" (undefined :: Bool))
	[ not False :: Bool
	, not True :: Bool
	]
	> grounds (value "&&" (&&) :$ var "p" (undefined :: Bool) :$ var "q" (undefined :: Bool))
	[ False && False :: Bool
	, False && True :: Bool
	, True && False :: Bool
	, True && True :: Bool
	]

To compute candidate generalizations from a given counter-example, we use the
following function:

	candidateGeneralizations :: Expr -> [Expr]
	candidateGeneralizations  =  map canonicalize
	                          .  concatMap canonicalVariations
	                          .  gen
	  where
	  gen e@(e1 :$ e2)  =
	    [holeAsTypeOf e | isListable e]
	    ++ [g1 :$ g2 | g1 <- gen e1, g2 <- gen e2]
	    ++ map (:$ e2) (gen e1)
	    ++ map (e1 :$) (gen e2)
	  gen e
	    | isVar e    =  []
	    | otherwise  =  [holeAsTypeOf e | isListable e]
	  isListable  =  not . null . tiersFor

The need for `isListable` above makes sure we only replace by variables what we
can enumerate.  Our candidate generalizations are listed in non-increasing
order of generality:

	> candidateGeneralizations (value "not" not :$ val False)
	[ p :: Bool
	, not p :: Bool
	]
	Prelude> candidateGeneralizations (value "||" (||) :$ val False :$ val True)
	[ p :: Bool
	, p || q :: Bool
	, p || p :: Bool
	, p || True :: Bool
	, False || p :: Bool
	]

For a given maximum number of tests, property and counter-example, the
following function returns a counter-example generalization if one is found.
It goes through the list of candidate generalizations and returns the first for
which all tests _fail_.

	counterExampleGeneralization :: Express a => Int -> (a -> Bool) -> Expr -> Maybe Expr
	counterExampleGeneralization maxTests prop e  =  listToMaybe
	  [g | g <- candidateGeneralizations e
	     , all (not . prop . evl) (take maxTests $ grounds g)]

We can finally define our `check` function, that will test a property and
report a counterexample and a generalization when either are found.

	check :: (Listable a, Express a) => (a -> Bool) -> IO ()
	check prop  =  putStrLn $ case counterExample 500 prop of
	  Nothing -> "+++ Tests passed.\n"
	  Just ce -> "*** Falsified, counterexample:  " ++ show ce
	          ++ case counterExampleGeneralization 500 prop ce of
	             Nothing -> ""
	             Just g -> "\n               generalization:  " ++ show g
	          ++ "\n"

Now we can find counterexamples and their generalizations:

	> check $ \xs -> sort (sort xs :: [Int]) == sort xs
	+++ Tests passed.

	> check $ \xs -> length (nub xs :: [Int]) == length xs
	*** Falsified, counterexample:  [0,0] :: [Int]
	               generalization:  x:x:xs :: [Int]

	> check $ \x -> x == x + (1 :: Int)
	*** Falsified, counterexample:  0 :: Int
	               generalization:  x :: Int

	> check $ \(x,y) -> x /= (y :: Int)
	*** Falsified, counterexample:  (0,0) :: (Int,Int)
	               generalization:  (x,x) :: (Int,Int)

The implementation above has some limitations:

* it only supports properties with one argument (uncurried);
* it only supports generalization of `Int`, `Bool`, `[Int]` and `[Bool]` values;
* there is no way to configure the number of test arguments.

Please see [Extrapolate] for a full-featured version without the above
limitations and with support for conditional generalizations.


Example 4: u-Speculate
----------------------

Using Haexpress, it takes less than 70 lines of code to define a function
`speculateAbout` that conjectures equations about a set of functions based on
the results of testing:

	> speculateAbout [hole (undefined :: Bool), val False, val True, value "not" not]
	[ not False == True :: Bool
	, not True == False :: Bool
	, not (not p) == p :: Bool
	]

	> speculateAbout -- TODO: sort

Please see ...



Further reading
---------------

For a detailed documentation, please see [Haexpress's Haddock documentation].


[Haexpress's Haddock documentation]: https://hackage.haskell.org/package/haexpress/docs/Data-Haexpress.html

[`Expr`]:         https://hackage.haskell.org/package/haexpress/docs/Data-Haexpress.html#t:val
[`val`]:          https://hackage.haskell.org/package/haexpress/docs/Data-Haexpress.html#v:val
[`evaluate`]:     https://hackage.haskell.org/package/haexpress/docs/Data-Haexpress.html#v:evaluate
[`$$`]:           https://hackage.haskell.org/package/haexpress/docs/Data-Haexpress.html#v:-36--36-
[`Show`]:         https://hackage.haskell.org/package/base/docs/Prelude.html#t:Show
[`Data.Dynamic`]: https://hackage.haskell.org/package/base/docs/Data-Dynamic.html

[LeanCheck]:   https://hackage.haskell.org/package/leancheck
[Extrapolate]: https://hackage.haskell.org/package/extrapolate
[Speculate]:   https://hackage.haskell.org/package/speculate

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
