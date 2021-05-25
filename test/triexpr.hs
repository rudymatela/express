import Test hiding (unit)
import Data.Express.Triexpr (Triexpr)
import qualified Data.Express.Triexpr as T


main :: IO ()
main  =  mainTest tests 10000


tests :: Int -> [Bool]
tests n =
  [ True

  , T.lookup one (T.fromList arithRules)
    == []

  , T.lookup (one -+- two) (T.fromList arithRules)
    == [ ([(yy, two), (xx, one)], yy -+- xx)
       ]

  , T.lookup ((one -+- two) -+- three) (T.fromList arithRules)
    == [ ([(yy, three), (xx, one -+- two)], yy -+- xx)
       , ([(zz, three), (yy, two), (xx, one)], xx -+- (yy -+- zz))
       ]

  , holds n $ \ees -> (sort . T.toList $ T.fromList ees) == sort (ees :: [(Expr,Int)])
  , holds n $ \ees -> (sort . T.toList $ T.fromList ees) == sort (ees :: [(Expr,Expr)])

  , holds n $ \e ees -> [e2 | (e1,e2) <- ees, _ <- maybeToList (e `match` e1)]
                     =$ sort $= map snd (T.lookup e (T.fromList ees) :: [([(Expr,Expr)],Expr)])

  -- TODO: test performance, lookup should be much faster than several
  --       `match`es
  ]


arithRules :: [(Expr,Expr)]
arithRules =
  [ abs' (abs' x)     >> abs' x
  , x -+- zero        >> x
  , x -*- one         >> x
  , x -*- zero        >> zero
  , x -+- y           >> y -+- x
  , (x -+- y) -+- z   >> x -+- (y -+- z)
  , (x -*- y) -*- z   >> x -*- (y -*- z)
  , (x -+- x) -*- y   >> x -*- (y -+- y)
  , x -*- (y -+- one) >> x -+- x -*- y
  , x -*- (y -+- z)   >> x -*- y -+- x -*- z

  -- bool rules --
  , p -&&- q          >> q -&&- p
  , p -||- q          >> q -||- p
  , not' (not' p)     >> p
  , p -&&- true       >> p
  , p -&&- false      >> false
  , p -||- true       >> true
  , p -||- false      >> p
  , (p -&&- q) -&&- r >> p -&&- (q -&&- r)
  , (p -||- q) -||- r >> p -||- (q -||- r)
  , not' (p -&&- q)   >> not' p -||- not' q
  , not' (p -||- q)   >> not' p -&&- not' q

  -- identities --
--, x                 >> x
--, p                 >> p
  ]
  where
  x = xx
  y = yy
  z = zz
  p = pp
  q = qq
  r = rr
  e1 >> e2 = (e1, e2)
  infix 0 >>
