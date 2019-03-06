TO DO list for haexpress
========================

List of things TO DO for haexpress

* add Char type to ListableExpr

* add variable operator `? :: Int -> Int -> Int` to `Data.Haexpress.Fixtures`
  (of course, as `-?- :: Expr -> Expr -> Expr`)

* 100% haddock coverage, including:
	- `>90%` example coverage
	- good module descriptions

* Add more tests of variable replacement

* Document `test/Test.ListableExpr` module throughly;

* partially replace `Test.Speculate.Expr` with this

* partially replace `Test.Extrapolate.Core` with this

* after using `haexpress` as a dependency of Speculate and Extrapolate
  remove any unused symbols from `Data.Haexpress.Fixtures`
