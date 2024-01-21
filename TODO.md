TO DO list for Express
======================

Nothing planned at the moment.

* Investigate the following bug:

	> (xx -:- yy -:- nil -++- yy -:- nil)
	[x,y,] ++ [y] :: [Int]

	> (xx -:- (yy -:- (nil -++- (yy -:- nil))))
	[x,y,] ++ [y] :: [Int]

  There's a dangling comma.  I think this ought to be displayed as:

	x:y:([] ++ y:nil)
