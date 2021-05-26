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

  , length allRules == 85
  , all isWellTyped $ map fst allRules
  , all isWellTyped $ map snd allRules

  , T.lookup zero  trie == []
  , T.lookup one   trie == []
  , T.lookup two   trie == []
  , T.lookup false trie == []
  , T.lookup true  trie == []

  , T.lookup (one -+- two) trie
    == [ (xx -+- yy, [(yy, two), (xx, one)], yy -+- xx)
       ]

  , T.lookup ((one -+- two) -+- three) trie
    == [ (xx -+- yy, [(yy, three), (xx, one -+- two)], yy -+- xx)
       , ((xx -+- yy) -+- zz, [(zz, three), (yy, two), (xx, one)], xx -+- (yy -+- zz))
       ]

  , T.lookup ((false -&&- false) -&&- true) trie
    == [ (pp -&&- qq, [(qq,true),(pp,false -&&- false)], qq -&&- pp)
       , (pp -&&- true, [(pp,false -&&- false)], pp)
       , ((pp -&&- qq) -&&- rr, [(rr,true),(qq,false),(pp,false)], pp -&&- (qq -&&- rr))
       ]

  , T.lookup (not' true) trie
    == [ (not' true, [], false) ]

  , T.lookup (true -||- true) trie
    == [ (pp -||- pp, [(pp,true)], pp)
       , (pp -||- qq, [(qq,true),(pp,true)], qq -||- pp)
       , (pp -||- true, [(pp,true)], true)
       , (true -||- pp, [(pp,true)], true)
       ]

  , holds n $ \ees -> (sort . T.toList $ T.fromList ees) == sort (ees :: [(Expr,Int)])
  , holds n $ \ees -> (sort . T.toList $ T.fromList ees) == sort (ees :: [(Expr,Expr)])

  , holds n $ \e ees -> [(e1,ms,e2 :: Expr) | (e1,e2) <- ees, ms <- maybeToList (e `match` e1)]
                     =$ sort $= T.lookup e (T.fromList ees)

  , holds n $ \e eus -> [(e1,ms,()) | (e1,()) <- sort eus, ms <- maybeToList (e `match` e1)]
                     == (T.lookup e (T.fromList eus))

  , holds n $ \e -> [(e1,ms,e2) | (e1,e2) <- allRules, ms <- maybeToList (e `match` e1)]
                 =$ sort $= T.lookup e trie
  ]
