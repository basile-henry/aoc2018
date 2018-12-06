import           Data.List
import           Data.Map.Strict (Map)
import           Puzzle

main :: IO ()
main = interact
  $ (++ "\n") . show
  . solve . sleepPattern
  . sortOn fst . getLedger . read

solve :: Map Int (Map Date Sleep) -> Int
solve
  = uncurry (*)
  . fmap snd
  . maximumWithKey fst
  . fmap (maximum . flip zip [0..] . sleepHistogram)
