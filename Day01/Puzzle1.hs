main = interact $ show . sum . map (read . removePlus) . lines

removePlus ('+':xs) = xs
removePlus xs       = xs
