import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Puzzle

main :: IO ()
main = interact
  $ (++ "\n") . show
  . solve . sleepPattern
  . sortOn fst . getLedger . read

solve :: Map Int (Map Date Sleep) -> Int
solve m = guardId * mostAsleep
  where
    timeSlept = fmap (sum . fmap (length . filter id . getSleep)) m

    (guardId, _) = maximumWithKey id timeSlept

    Just sleep = Map.lookup guardId m

    mostAsleep
      = snd
      . maximum
      . flip zip [0..]
      $ sleepHistogram sleep
