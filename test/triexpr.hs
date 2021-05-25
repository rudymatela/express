import Test
import Data.Express.Triexpr (Triexpr)
import qualified Data.Express.Triexpr as T


main :: IO ()
main  =  mainTest tests 10000


tests :: Int -> [Bool]
tests n =
  [ True

  , length allRules == 45

  , T.lookup zero  (T.fromList allRules) == []
  , T.lookup one   (T.fromList allRules) == []
  , T.lookup two   (T.fromList allRules) == []
  , T.lookup false (T.fromList allRules) == []
  , T.lookup true  (T.fromList allRules) == []

  , T.lookup (one -+- two) (T.fromList allRules)
    == [ ([(yy, two), (xx, one)], yy -+- xx)
       ]

  , T.lookup ((one -+- two) -+- three) (T.fromList allRules)
    == [ ([(yy, three), (xx, one -+- two)], yy -+- xx)
       , ([(zz, three), (yy, two), (xx, one)], xx -+- (yy -+- zz))
       ]

  , holds n $ \ees -> (sort . T.toList $ T.fromList ees) == sort (ees :: [(Expr,Int)])
  , holds n $ \ees -> (sort . T.toList $ T.fromList ees) == sort (ees :: [(Expr,Expr)])

  , holds n $ \e ees -> [(ms,e2) | (e1,e2) <- ees, ms <- maybeToList (e `match` e1)]
                     =$ sort $= (T.lookup e (T.fromList ees) :: [([(Expr,Expr)],Expr)])

  , holds n $ \e eus -> [(ms,()) | (e1,()) <- sort eus, ms <- maybeToList (e `match` e1)]
                     == (T.lookup e (T.fromList eus) :: [([(Expr,Expr)],())])

  -- TODO: test performance, lookup should be much faster than several
  --       `match`es
  ]
