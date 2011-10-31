module Aufgabe2 ( istPrimal ) where

-- Beispiel 1
istPrimal :: Integer -> Bool
istPrimal n
    | notElem n p = False
    | otherwise = hasNoFactors n p
        where 
            p = [1 + x * 4 | x <- [1..(n `div` 4)]]
            hasNoFactors n p = (secondFactorForLastIn p) `notElem` p
            secondFactorForLastIn [] = -1
            secondFactorForLastIn p@(x:xs)
                | (last p) `mod` x == 0 = (last p) `div` x
                | otherwise = secondFactorForLastIn xs

-- Beispiel 2
faktorisiere :: Integer -> [(Integer,Integer)]
faktorisiere x
    | istPrimal x = []
    | otherwise = [(x,x)]


-- Beispiel 3
type Editor = String
type Suchzeichenreihe = String
type Index = Integer
type Vorkommen = Integer
type Alt = String
type Neu = String

suche :: Editor -> Suchzeichenreihe -> Index
suche e s = -1

-- Beispiel 4
sucheAlle :: Editor -> Suchzeichenreihe -> [Index]
sucheAlle e s = []

-- Beispiel 5
ersetze :: Editor -> Vorkommen -> Alt -> Neu -> Editor
ersetze e i _ _
    | i < 0 = e
ersetze e i s t = []