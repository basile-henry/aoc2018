import           Data.Either
import           Data.IntSet
import           Control.Monad

import           Debug.Trace

main = interact $ show . solve . fmap (read . removePlus) . lines

removePlus ('+':xs) = xs
removePlus xs       = xs

solve :: [Int] -> Int
solve
  = fromLeft (error "Unreachable")
  . foldM step (0, singleton 0)
  . cycle
  where
    step (frequency, visited) change
      -- We found it!
      | newFrequency `member` visited
      = Left newFrequency

      | otherwise
      = Right (newFrequency, insert newFrequency visited)

      where
        newFrequency = frequency + change
