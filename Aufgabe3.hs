module Aufgabe3 ( anp1, anp2 ) where

type Matrix = [[Integer]]
type Zeilen = Integer
type Spalten = Integer
type Fuellwert = Integer

-- Beispiel 1
anp1 :: [[Integer]] -> Matrix
anp1 [] = [[1]]
anp1 compLists = [x ++ replicate (maxLength - length x) 0 | x <- compLists]
    where maxLength = maximum (map length compLists)

-- Beispiel 2
anp2 :: [[Integer]] -> Zeilen -> Spalten -> Fuellwert -> Matrix
anp2 _ 0 _ _ = []
anp2 _ _ 0 _ = []
anp2 l z s w 
    | l == [] = replicate (fromIntegral z) (replicate (fromIntegral s) w)
    | otherwise = take (fromInteger z) [cropOrExtendCols x | x <- l]
        where 
            cropOrExtendCols x
                | length x < (fromIntegral s) = x ++ replicate ((fromIntegral s) - length x) w
                | otherwise = take (fromIntegral s) x

