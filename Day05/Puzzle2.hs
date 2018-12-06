import           Data.Char
import qualified Data.IntSet  as Set

main :: IO ()
main = interact $ (++ "\n") . show . solve . init

reduce :: [Int] -> Int
reduce = length . foldr step []
  where
    step c []     = pure c
    step c (r:rev)
      | check r c = rev
      | otherwise = c:r:rev

check :: Int -> Int -> Bool
check x y = abs (x - y) == 32

solve :: String -> Int
solve polymer = minimum $ map (reduce . flip removeUnit polymer') units
  where
    polymer' = map ord polymer

    units = Set.toList . Set.fromList $ map (ord . toLower) polymer

    removeUnit c = filter (check c)
