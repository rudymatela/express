Express
=======

[![Express's Build Status][build-status]][build-log]
[![Express on Hackage][hackage-version]][express-on-hackage]
[![Express on Stackage LTS][stackage-lts-badge]][express-on-stackage-lts]
[![Express on Stackage Nightly][stackage-nightly-badge]][express-on-stackage-nightly]

![Express logo][express-logo]


Express is a library for manipulating dynamically typed Haskell expressions.
It's like [`Data.Dynamic`] but with support for encoding applications and
variables.

It provides the [`Expr`] type and over a hundred functions for
building, evaluating, comparing, folding, canonicalizing and matching
[`Expr`]s.  See [Express's Haddock documentation] for more details.

This library has been used in the implementation of
[Speculate] and [Extrapolate].


Installing
----------

To install the latest Express version from Hackage, just run:

	$ cabal update
	$ cabal install express


Basics
------

To import `Express` just:

	> import Data.Express

For types that are [`Show`] instances,
we can use [`val`] to encode values as [`Expr`]s.

	> let false = val False
	> :t false
	false :: Expr
	> print false
	False :: Bool

	> let one = val (1 :: Int)
	> :t one
	one :: Expr
	> print one
	1 :: Int

As seen above, the [`Show`] instance for [`Expr`] produces a string with the
encoded value and it's type.

For types that aren't [`Show`] instances, like functions,
we can use [`value`] to encode values as [`Expr`]s.

	> let notE = value "not" not
	> :t notE
	notE :: Expr
	> print notE
	not :: Bool -> Bool

Using [`:$`] we can apply function valued [`Expr`]s, to other Exprs.

	> let notFalse = notE :$ false
	> :t notFalse
	notFalse :: Expr
	> notFalse
	not False :: Bool

Using [`evaluate`] and [`eval`] we can evaluate [`Expr`]s back into a regular Haskell value.

	> evaluate notFalse :: Maybe Bool
	Just True
	> evaluate notFalse :: Maybe Int
	Nothing
	> eval False notFalse
	True
	> eval (0::Int) notFalse
	0


Example 1: heterogeneous lists
------------------------------

Like with [`Data.Dynamic`], we can use Express to create heterogeneous lists.

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

[u-Extrapolate] is a property-based testing library
capable of generalizing counter-examples.  It's implementation has under 40
lines of code.  Besides, using Express to encode expressions, it uses
[LeanCheck] for generating test values.

	import Data.Express
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

[u-Extrapolate] has some limitations:

* it only supports properties with one argument (uncurried);
* it only supports generalization of `Int`, `Bool`, `[Int]` and `[Bool]` values;
* there is no way to configure the number of test arguments.

Please see [Extrapolate] for a full-featured version without the above
limitations and with support for conditional generalizations.


Example 4: u-Speculate
----------------------

Using Express, it takes less than 70 lines of code to define a function
`speculateAbout` that conjectures equations about a set of functions based on
the results of testing:

	> speculateAbout [hole (undefined :: Bool), val False, val True, value "not" not]
	[ not False == True :: Bool
	, not True == False :: Bool
	, not (not p) == p :: Bool
	]

	> speculateAbout
    >   [ hole (undefined :: Int)
    >   , hole (undefined :: [Int])
    >   , val ([] :: [Int])
    >   , value ":" ((:) :: Int -> [Int] -> [Int])
    >   , value "++" ((++) :: [Int] -> [Int] -> [Int])
    >   , value "sort" (sort :: [Int] -> [Int])
    >   ]
	[ sort [] == [] :: Bool
    , xs ++ [] == xs :: Bool
    , [] ++ xs == xs :: Bool
    , sort (sort xs) == sort xs :: Bool
    , sort [x] == [x] :: Bool
    , [x] ++ xs == x:xs :: Bool
    , sort (xs ++ ys) == sort (ys ++ xs) :: Bool
    , sort (x:sort xs) == sort (x:xs) :: Bool
    , sort (xs ++ sort ys) == sort (xs ++ ys) :: Bool
    , sort (sort xs ++ ys) == sort (xs ++ ys) :: Bool
    , (x:xs) ++ ys == x:(xs ++ ys) :: Bool
    , (xs ++ ys) ++ zs == xs ++ (ys ++ zs) :: Bool
	]

Please see the [u-Speculate] example in the [eg](eg) folder for the full code
of `speculateAbout`.

[u-Speculate] has some limitations:

* it sometimes prints redundant equations;
* although it usually runs quickly with less than 6 symbols,
  runtime is exponential with the number of symbols given,
  providing it with more than a dozen symbols can make it run for several
  minutes or hours;
* there is no way to configure the size limit of reported equations;
* it only supports variables of `Int`, `Bool`, `[Int]`, and `[Bool]` types.

Please see [Speculate] for a full-featured version without the above
limitations.


Further reading
---------------

For a detailed documentation, please see [Express's Haddock documentation].

For more examples, see the [eg](eg) and [bench](bench) folders.


[Express's Haddock documentation]: https://hackage.haskell.org/package/express/docs/Data-Express.html

[`Expr`]:         https://hackage.haskell.org/package/express/docs/Data-Express.html#t:Expr
[`val`]:          https://hackage.haskell.org/package/express/docs/Data-Express.html#v:val
[`value`]:        https://hackage.haskell.org/package/express/docs/Data-Express.html#v:value
[`eval`]:         https://hackage.haskell.org/package/express/docs/Data-Express.html#v:eval
[`evaluate`]:     https://hackage.haskell.org/package/express/docs/Data-Express.html#v:evaluate
[`:$`]:           https://hackage.haskell.org/package/express-0.1.1/docs/Data-Express.html#v::-36-
[`$$`]:           https://hackage.haskell.org/package/express/docs/Data-Express.html#v:-36--36-
[`Show`]:         https://hackage.haskell.org/package/base/docs/Prelude.html#t:Show
[`Data.Dynamic`]: https://hackage.haskell.org/package/base/docs/Data-Dynamic.html

[LeanCheck]:   https://hackage.haskell.org/package/leancheck
[Extrapolate]: https://hackage.haskell.org/package/extrapolate
[Speculate]:   https://hackage.haskell.org/package/speculate

[u-Speculate]:   eg/u-speculate.hs
[u-Extrapolate]: eg/u-extrapolate.hs

[express-logo]: https://github.com/rudymatela/express/raw/master/doc/express.svg?sanitize=true

[build-status]: https://travis-ci.org/rudymatela/express.svg?branch=master
[build-log]:    https://travis-ci.org/rudymatela/express
[hackage-version]: https://img.shields.io/hackage/v/express.svg
[express-on-hackage]: https://hackage.haskell.org/package/express
[stackage-lts-badge]:          https://stackage.org/package/express/badge/lts
[stackage-nightly-badge]:      https://stackage.org/package/express/badge/nightly
[express-on-stackage]:         https://stackage.org/package/express
[express-on-stackage-lts]:     https://stackage.org/lts/package/express
[express-on-stackage-nightly]: https://stackage.org/nightly/package/express
