import Data.Char

main :: IO ()
main = interact $ (++ "\n") . show . reduce . init

reduce :: String -> Int
reduce = length . foldr step ""
  where
    step c ""      = pure c
    step c (r:rev)
      | check r c = rev
      | otherwise = c:r:rev

    check x y = abs (ord x - ord y) == 32
