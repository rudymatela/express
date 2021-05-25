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
  [ abs' (abs' xx)      -=- abs' xx
  , xx -+- zero         -=- xx
  , xx -*- one          -=- xx
  , xx -*- zero         -=- zero
  , xx -+- yy           -=- yy -+- xx
  , (xx -+- yy) -+- zz  -=- xx -+- (yy -+- zz)
  , (xx -*- yy) -*- zz  -=- xx -*- (yy -*- zz)
  , (xx -+- xx) -*- yy  -=- xx -*- (yy -+- yy)
  , xx -*- (yy -+- one) -=- xx -+- xx -*- yy
  , xx -*- (yy -+- zz)  -=- xx -*- yy -+- xx -*- zz
--, xx -=- xx

  -- bool rules --
  , pp -&&- qq           -=- qq -&&- pp
  , pp -||- qq           -=- qq -||- pp
  , not' (not' pp)       -=- pp
  , pp -&&- true         -=- pp
  , pp -&&- false        -=- false
  , pp -||- true         -=- true
  , pp -||- false        -=- pp
  , (pp -&&- qq) -&&- rr -=- pp -&&- (qq -&&- rr)
  , (pp -||- qq) -||- rr -=- pp -||- (qq -||- rr)
  , not' (pp -&&- qq)    -=- not' pp -||- not' qq
  , not' (pp -||- qq)    -=- not' pp -&&- not' qq
--, pp -=- pp
  ]

(-=-) :: Expr -> Expr -> (Expr,Expr)
e1 -=- e2 = (e1, e2)
infix 0 -=-
