import           Data.Bool
import           Data.Char
import           Data.List
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Text.ParserCombinators.ReadP

data Claim =
  Claim
    { i :: Int
    , x :: Int
    , y :: Int
    , w :: Int
    , h :: Int
    } deriving Show

main = interact $ (++ "\n") . show . solve Map.empty . fmap parse . lines

parse :: String -> Claim
parse = fst . head . readP_to_S claim

claim :: ReadP Claim
claim =
  Claim
    <$  char '#'
    <*> int
    <*  string " @ "
    <*> int
    <*  char ','
    <*> int
    <*  string ": "
    <*> int
    <*  char 'x'
    <*> int

int :: ReadP Int
int = read <$> munch1 isDigit

solve :: Map (Int, Int) [Int] -> [Claim] -> Int
solve m []     =
  let overlaping = nub . sort . concat . map snd . Map.toList $ Map.filter ((> 1) . length) m
  in  maybe (error "Not found") snd . find (uncurry (/=)) $ zip overlaping [1..]
solve m (c:cs) = solve (insertClaim c m) cs

insertClaim :: Claim -> Map (Int, Int) [Int] -> Map (Int, Int) [Int]
insertClaim c m =
  foldr
    (Map.alter (Just . maybe [i c] (i c :)))
    m
    [ (x c + i, y c + j) | i <- [0..w c - 1], j <- [0..h c - 1] ]
