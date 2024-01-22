TO DO list for Express
======================

Nothing major planned at the moment.

* Investigate the list pretty-printing bug described below.


## List pretty-printing bug

	> xx -:- yy -:- nil -++- yy -:- nil
	[x,y,] ++ [y] :: [Int]

	> xx -:- (yy -:- (nil -++- (yy -:- nil)))
	[x,y,] ++ [y] :: [Int]

There's a dangling comma.  This ought to be displayed as:

	x:y:([] ++ [y])

The second argument of `++` does not really matter to expose the bug:

	> xx -:- yy -:- nil -++- is_
	[x,y,] ++ _ :: [Int]

A couple commented-out tests have been added to `test/show.hs`.
