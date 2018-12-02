import Control.Arrow
import Data.Bool
import Data.List

main = interact $ (++ "\n") . show . solve . lines

solve :: [String] -> Int
solve = uncurry (*) . (sum *** sum) . unzip . map (repeated 2 &&& repeated 3)

repeated n = bool 0 1 . elem n . map length . group . sort
