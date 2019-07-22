TO DO list for haexpress
========================

Here is a list of things TO DO for haexpress:

* _new naming standard_ instead of naming plain 'Expr's as somethingE.
  Whenever the function encoded is polymorphic, use the type instead of `E`.
  For example: justInt and justBool are respectively the Exprs representing the
  Just constructors for the types int and bool.
  Later on I can decide if I'll rename notE to notBool.

* _explain new naming standard_ on Fixtures, explain the new naming standard
  described above.

* on fixtures, add:

	- nilInt
	- nilBool
	- nilChar
	- nothing
	- nothingInt
	- nothingBool
	- ... and others whereever possible

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
