-- Beispiel 1
istPrimal :: Integer -> Bool
istPrimal n
    | notElem n (pFor n) = False
    | otherwise = hasNoFactors n (pFor n)
        where 
            pFor n = [1 + x * 4 | x <- [1..n]]
            hasNoFactors n p =  notElem (factorFor (last p) p) p
            factorFor _ [] = -1
            factorFor lastInP (x:xs)
                | mod lastInP x == 0 = div lastInP x
                | otherwise = factorFor lastInP xs


foo n = [1 + x * 4 | x <- [1..n]]

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