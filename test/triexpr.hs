import Test
import Data.Express.Triexpr (Triexpr)
import qualified Data.Express.Triexpr as T


trie :: Triexpr Expr
trie  =  T.fromList allRules


main :: IO ()
main  =  mainTest tests 10000


tests :: Int -> [Bool]
tests n =
  [ True

  , length allRules == 72
  , all isWellTyped $ map fst allRules
  , all isWellTyped $ map snd allRules

  , T.lookup zero  trie == []
  , T.lookup one   trie == []
  , T.lookup two   trie == []
  , T.lookup false trie == []
  , T.lookup true  trie == []

  , T.lookup (one -+- two) trie
    == [ ([(yy, two), (xx, one)], yy -+- xx)
       ]

  , T.lookup ((one -+- two) -+- three) trie
    == [ ([(yy, three), (xx, one -+- two)], yy -+- xx)
       , ([(zz, three), (yy, two), (xx, one)], xx -+- (yy -+- zz))
       ]

  , T.lookup ((false -&&- false) -&&- true) trie
    == [ ([(qq,true),(pp,false -&&- false)], qq -&&- pp)
       , ([(pp,false -&&- false)], pp)
       , ([(rr,true),(qq,false),(pp,false)], pp -&&- (qq -&&- rr))
       ]

  , T.lookup (not' true) trie
    == [ ([], false) ]

  , T.lookup (true -||- true) trie
    == [ ([(pp,true)], pp)
       , ([(qq,true),(pp,true)], qq -||- pp)
       , ([(pp,true)], true)
       ]

  , holds n $ \ees -> (sort . T.toList $ T.fromList ees) == sort (ees :: [(Expr,Int)])
  , holds n $ \ees -> (sort . T.toList $ T.fromList ees) == sort (ees :: [(Expr,Expr)])

  , holds n $ \e ees -> [(ms,e2) | (e1,e2) <- ees, ms <- maybeToList (e `match` e1)]
                     =$ sort $= (T.lookup e (T.fromList ees) :: [([(Expr,Expr)],Expr)])

  , holds n $ \e eus -> [(ms,()) | (e1,()) <- sort eus, ms <- maybeToList (e `match` e1)]
                     == (T.lookup e (T.fromList eus) :: [([(Expr,Expr)],())])

  , holds n $ \e -> [(ms,e2) | (e1,e2) <- allRules, ms <- maybeToList (e `match` e1)]
                 =$ sort $= T.lookup e trie

  -- TODO: test performance, lookup should be much faster than several
  --       `match`es
  ]
