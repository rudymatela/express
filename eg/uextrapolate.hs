-- Copyright 2019 Rudy Matela
--
-- micro extrapolate
import Data.List
import Data.Maybe
import Data.Haexpress
import Test.LeanCheck hiding (counterExample, check)

main :: IO ()
main  =  do
  check $ \xs -> sort (sort xs :: [Int]) == sort xs
  check $ \xs -> length (nub xs :: [Int]) == length xs
  check $ \(x,y) -> x + y == y + (x :: Int)
  check $ \(x,y) -> x + y == x + (x :: Int)


check :: (Listable a, Express a) => (a -> Bool) -> IO ()
check prop  =  putStrLn $ case counterExample 500 prop of
  Nothing -> "+++ Tests passed."
  Just ce -> "*** Falsified, counterexample:  " ++ show ce
          ++ case counterExampleGeneralization 500 prop ce of
             Nothing -> ""
             Just g -> "\n               generalization:  " ++ show g


counterExample :: (Listable a, Express a) => Int -> (a -> Bool) -> Maybe Expr
counterExample maxTests prop  =  listToMaybe
  [expr x | x <- take maxTests list, not (prop x)]

counterExampleGeneralization :: Express a => Int -> (a -> Bool) -> Expr -> Maybe Expr
counterExampleGeneralization maxTests prop e  =  listToMaybe
  [g | g <- candidateGeneralizations e
     , all (not . prop . evl) (take maxTests $ grounds g)]


candidateGeneralizations :: Expr -> [Expr]
candidateGeneralizations  =  map canonicalize
                          .  concatMap canonicalVariations
                          .  candidateHoleGeneralizations

candidateHoleGeneralizations :: Expr -> [Expr]
candidateHoleGeneralizations  =  gen
  where
  gen e@(e1 :$ e2)  =
    [holeAsTypeOf e | isListable e]
    ++ [g1 :$ g2 | g1 <- gen e1, g2 <- gen e2]
    ++ map (:$ e2) (gen e1)
    ++ map (e1 :$) (gen e2)
  gen e
    | isVar e    =  []
    | otherwise  =  [holeAsTypeOf e | isListable e]
  isListable  =  not . null . tiersFor

grounds :: Expr -> [Expr]
grounds e = (e //-)
        <$> concat (products [mapT ((,) v) (tiersFor v) | v <- nubVars e])

tiersFor :: Expr -> [[Expr]]
tiersFor e  =  case show (typ e) of
  "Int"    ->  mapT val (tiers `asTypeOf` [[undefined :: Int]])
  "Bool"   ->  mapT val (tiers `asTypeOf` [[undefined :: Bool]])
  "[Int]"  ->  mapT val (tiers `asTypeOf` [[undefined :: [Int]]])
  "[Bool]" ->  mapT val (tiers `asTypeOf` [[undefined :: [Bool]]])
  _        ->  []
