TO DO list for haexpress
========================

Here is a list of things TO DO for haexpress:

* FIXME:

  ```
  *All Prelude> hole (undefined :: Bool->Bool) :$ hole (undefined::Bool)
  *** Exception: src/Data/Haexpress/Utils/String.hs:89:1-59: Non-exhaustive patterns in function isInfix
  ```

* document and test the `Instances` module

* document and test recently added stuff

* sweep occurrences of `TODO:` throughout source files

* 100% haddock coverage, including:
	- `>90%` example coverage
	- good module descriptions

* Document `test/Test.ListableExpr` module thoroughly;


only then
---------

* write the README

* write a tutorial on the `Data.Haexpress` module's haddock

* review `Data.Haexpress` haddock

* release on GitHub

* release on Hackage -- this should coincide with a new release for both
  Speculate and Extrapolate.
