module Aufgabe3 ( anp1 ) where

type Matrix = [[Integer]]

-- Beispiel 1
anp1 :: [[Integer]] -> Matrix
anp1 [] = [[1]]
anp1 cls = [x ++ replicate (maxLength - length x) 0 | x <- cls]
    where maxLength = maximum (map length cls)