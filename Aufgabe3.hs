module Aufgabe3 ( anp1 ) where

type Matrix = [[Integer]]

-- Beispiel 1
anp1 :: [[Integer]] -> Matrix
anp1 [] = [[1]]
anp1 cls
    | length cls == 1 = cls
    | otherwise = [[-1]]