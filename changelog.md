Changelog for Express
=====================


upcoming
--------

* No changes in the main API

* `Data.Fixtures`: support more types in existing functions

* `Data.Fixtures`: add `filter'`, `drop'`, `take'`, `foldr'`, `ff2`, `ff3`, ...


v1.0.14 (January 2024)
----------------------

* `Data.Express`: add `>$$<`, `>$$` and `$$<`.

* fix pretty-printing bug:
  an expression encoding `x:y:([] ++ _) :: [Int]`
  was being displayed as `[x,y,] ++ _ :: [Int]`.

* `Data.Express.Fixtures`: update `-..`, `--..` and `--..-`.

* improve pretty-printing

* make ordering of `typesIn` consistent between GHC 9.8 and earlier versions

* fix a test failure on GHC 9.6 (previous GHC versions unaffected)

* simplify and improve testing, new benchmark and minor updates


v1.0.12 (July 2023)
-------------------

* make ordering of `typesIn` consistent between GHC 9.6 and earlier versions

* fix a test failure on GHC 9.6 (previous GHC versions unaffected)

* drop support for GHC 8.0, GHC 7.10 and GHC 7.8.
  The current version will still work in these,
  but these are not run on CI anymore
  and future versions will no longer be tested.

* miscellaneous improvements in build and CI scripts


v1.0.10 (April 2022)
--------------------

* show function-encoded Ordering case expressions exceptionally

* show function-encoded Bool case expressions exceptionally

* add `caseBool` and `caseOrdering` to `Data.Express.Fixtures`

* minor updates in Makefile and CI scripts


v1.0.8 (September 2021)
-----------------------

* `Data.Express.Express.Derive`:
  fix generation of `-:` and `->:` in earlier GHC's.

* `Data.Express.Utils.TH`:
  add `unboundVars`, `toBounded` and `toBoundedQ`.


v1.0.6 (September 2021)
-----------------------

* fix pretty printing of unapplied infixed variable functions:
  use `f :: ...`  instead of ``(`f`) :: ...``

* `Data.Express.Fixtures`:
  add `init'`, `div'`, `mod'`, `quot'`, `rem'`, `question` and `oo`.

* minor fixes in README


v1.0.4 (July 2021)
------------------

* deeply encode `Ratio`s
* add `Express (Complex a)` instance
* add several missing `Name` instances
* `deriveName` now uses `x` for `Num` instances


v1.0.2 (July 2021)
------------------

* more Express instances:
	- `Double` & `Float`
	- `Int*` types from `Data.Int`
	- `Word*` types from `Data.Word`
	- `GeneralCategory` from `Data.Char`

* minor fix in README


v1.0.0 (July 2021)
------------------

This release indicates that the `Data.Express` API is now stable.

* no changes since v0.2.0 or v0.1.16.


v0.2.0 (July 2021)
------------------

This release indicates that the `Data.Express` API is stable.

* no changes since v0.1.16


v0.1.16 (July 2021)
-------------------

* add `five`, `six`, ... `twelve` to `Data.Express.Fixtures`.

* add `cs_` to `Data.Express.Fixtures`.

* improve backwards compatibility:
  `Data.Express.Core/Hole/Match/Map/Name/Triexpr/Utils` now work on Hugs.

* 100% Haddock coverage on most modules including REPL examples.


v0.1.14 (June 2021)
-------------------

* permit and pretty-print `[<n>..<m>]` notations.

* improve default variable names when canonicalizing
	- lists are named xs, ys, xss, yss, etc.
	- functions are named f, g, h
	- before they were simply x, y, z


v0.1.12 (May 2021)
------------------

* `Data.Express.Fixtures`, add several symbols:
	- `hh` and `hhE`;
	- `four` and `zzs`;
	- `signum'` and `signumE`;
	- `compose` and `-.-`;
	- `mapE` and `map'`.

* Add the experimental `Triexpr` module, including:
	- the `Triexpr` type;
	- tests;
	- benchmarks.

* Retire Travis as the CI


v0.1.10 (May 2021)
------------------

* add the `hasHole` and `isComplete` functions
* add the `encompasses` function
* add `appendInt` to `Data.Express.Fixtures`
* add the `u-conjure` example
* the `Express` typeclass now requires `Show`
* improve examples in the `eg/` folder
* improve tests of `hasInstanceOf` and `isInstanceOf`
* improve tests
* add this changelog


v0.1.8 (April 2021)
-------------------

* slightly change behaviour of `canonicalVariations` and related functions.
* add more fixtures and improve fixtures' documentation
* improve Makefile and test scripts
* use GitHub actions as CI


v0.1.6 (April 2021)
-------------------

* add `compareLexicographically` and `compareQuickly`
* define behaviour of `canonicalVariations` for some undefined cases
* improve haddock documentation
* improve tests


v0.1.4 (April 2021)
-------------------

* add the `fill` and `isFun` functions
* `Data.Express.Fixtures`: more fixtures, define fixity
* add fixity for some fixtures
* improve documentation, tests and lint


v0.1.3 (March 2020)
-------------------

See the git commit log for v0.1.3 and previous versions
down to as early as February 2019.
