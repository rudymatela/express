TO DO list for express
======================

Here is a list of things TO DO for express:

* fix `O (n log n)` text on functions that use `+++`.  Mention that this is
  only true on "shallow" expressions.

* consider declaring a `Num` instance for `Expr` on `Fixtures`,
  so one could write `0 + 1` to mean `zero -+- one`.

* review Haddock of `Data.Express`

* document the `Instances` module,
  including note about linear time on searches

* test the `Instances` module

* sweep occurrences of `TODO:` throughout source files

* Document `test/Test.ListableExpr` module thoroughly;
