import Test hiding (unit)
import Data.Express.Triexpr (Triexpr)
import qualified Data.Express.Triexpr as T


main :: IO ()
main  =  mainTest tests 10000


tests :: Int -> [Bool]
tests n =
  [ True

  , T.lookup zero (T.fromList allRules) == []
  , T.lookup one  (T.fromList allRules) == []
  , T.lookup two  (T.fromList allRules) == []

  , T.lookup (one -+- two) (T.fromList allRules)
    == [ ([(yy, two), (xx, one)], yy -+- xx)
       ]

  , T.lookup ((one -+- two) -+- three) (T.fromList allRules)
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


allRules :: [(Expr,Expr)]
allRules  =  intRules ++ boolRules

intRules :: [(Expr,Expr)]
intRules  =
  [ abs' (abs' xx)       -=- abs' xx
  , negate' (negate' xx) -=- xx
  , xx -+- zero          -=- xx
  , xx -*- one           -=- xx
  , xx -*- zero          -=- zero
  , xx -+- yy            -=- yy -+- xx
  , (xx -+- yy) -+- zz   -=- xx -+- (yy -+- zz)
  , (xx -*- yy) -*- zz   -=- xx -*- (yy -*- zz)
  , (xx -+- xx) -*- yy   -=- xx -*- (yy -+- yy)
  , xx -*- (yy -+- one)  -=- xx -+- xx -*- yy
  , xx -*- (yy -+- zz)   -=- xx -*- yy -+- xx -*- zz
--, xx -=- xx
  ]

boolRules :: [(Expr,Expr)]
boolRules  =
  [ pp -&&- qq           -=- qq -&&- pp
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
