Changelog for Express
=====================


v0.1.14
-------

* permit and pretty-print `[<n>..<m>]` notations.

* improve default variable names when canonicalizing
	- lists are named xs, ys, xss, yss, etc.
	- functions are named f, g, h
	- before they were simply x, y, z


v0.1.12
-------

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


v0.1.10
-------

* add the `hasHole` and `isComplete` functions
* add the `encompasses` function
* add `appendInt` to `Data.Express.Fixtures`
* add the `u-conjure` example
* the `Express` typeclass now requires `Show`
* improve examples in the `eg/` folder
* improve tests of `hasInstanceOf` and `isInstanceOf`
* improve tests
* add this changelog


v0.1.8
------

* slightly change behaviour of `canonicalVariations` and related functions.
* add more fixtures and improve fixtures' documentation
* improve Makefile and test scripts
* use GitHub actions as CI


v0.1.6
------

* add `compareLexicographically` and `compareQuickly`
* define behaviour of `canonicalVariations` for some undefined cases
* improve haddock documentation
* improve tests


v0.1.4
------

* add the `fill` and `isFun` functions
* `Data.Express.Fixtures`: more fixtures, define fixity
* add fixity for some fixtures
* improve documentation, tests and lint


v0.1.3
------

See the git commit log for v0.1.3 and previous versions.
