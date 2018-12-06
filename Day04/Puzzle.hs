{-# LANGUAGE NamedFieldPuns #-}

module Puzzle where

import           Data.Bool
import           Data.Char
import           Data.List
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Text.ParserCombinators.ReadP

data Date =
  Date
    { year  :: Int
    , month :: Int
    , day   :: Int
    } deriving (Show, Eq, Ord)

data Time =
  Time
    { hour :: Int
    , mins :: Int
    } deriving (Show, Eq, Ord)

data Entry
  = Shift Int
  | Asleep
  | WakeUp
  deriving (Show)

newtype Ledger =
  Ledger { getLedger :: [((Date, Time), Entry)] }
    deriving (Show)

instance Read Date where
  readsPrec _ = readP_to_S $
    Date <$> int <* char '-' <*> int <* char '-' <*> int

instance Read Time where
  readsPrec _ = readP_to_S $
    Time <$> int <* char ':' <*> int

instance Read Entry where
  readsPrec _ = readP_to_S $
    choice
      [ Shift  <$ string "Guard #" <*> int <* string " begins shift"
      , Asleep <$ string "falls asleep"
      , WakeUp <$ string "wakes up"
      ]

instance Read Ledger where
  readsPrec _ = readP_to_S $
    let entry =
          (,)
            <$  char '['
            <*> ((,)
                  <$> readS_to_P reads
                  <*  char ' '
                  <*> readS_to_P reads)
            <*  char ']'
            <*  char ' '
            <*> readS_to_P reads

    in Ledger <$> entry `sepBy` char '\n'

int :: ReadP Int
int = read <$> many1 (satisfy isDigit)

newtype Sleep = Sleep { getSleep :: [Bool] } deriving Show

newSleep :: Sleep
newSleep = Sleep (replicate 60 False)

asleep :: Int -> Sleep -> Sleep
asleep m (Sleep s) = Sleep (take m s ++ replicate (60 - m) True)

awake :: Int -> Sleep -> Sleep
awake m (Sleep s) = Sleep (take m s ++ replicate (60 - m) False)

midnight :: Date -> Time -> Date
midnight d@Date { day } Time{ hour }
  | hour == 23 = d {day = day + 1}
  | otherwise  = d

sleepPattern :: [((Date, Time), Entry)] -> Map Int (Map Date Sleep)
sleepPattern = snd . foldl' step (Nothing, Map.empty)
  where
    step (_, m) ((d, t), Shift i) =
      let k = midnight d t
          v = newSleep
      in
        ( Just i
        , Map.alter (Just . maybe (Map.singleton k v) (Map.insert k v)) i m
        )
    step (Just i, m) ((d, t), e) =
      let sleep =
            (case e of Asleep -> asleep; WakeUp -> awake; _ -> undefined)
              (mins t)
      in
        ( Just i
        , Map.adjust (Map.adjust sleep d) i m
        )
    step (Nothing, _) inp = error $ "We have no guard at: " ++ show inp

sleepHistogram :: Map k Sleep -> [Int]
sleepHistogram =
  foldr (\i -> zipWith (+) (bool 0 1 <$> getSleep i)) (repeat 0) . Map.elems

maximumWithKey :: (Ord b, Bounded a) => (a -> b) -> Map k a -> (k, a)
maximumWithKey f =
  Map.foldrWithKey
    (\i t (gi, gt) -> if f t > f gt then (i, t) else (gi, gt))
    (undefined, minBound)
