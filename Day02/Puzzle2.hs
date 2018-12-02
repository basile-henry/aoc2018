import Data.Bool
import Data.List

main = interact $ (++ "\n") . show . solve . lines

solve :: [String] -> String
solve [] = error "Not found"
solve (str:strs)
  | Just i <- findIndex ((== 1) . diff str) strs
  = same str (strs !! i)

  | otherwise
  = solve strs

same :: String -> String -> String
same a b = map fst . filter (uncurry (==)) $ zip a b

diff :: String -> String -> Int
diff a b = sum $ zipWith (\x -> bool 1 0 . (== x)) a b
