-- Beispiel 1
istPrimal :: Integer -> Bool
istPrimal n = True --foo (p n) n

p n = [ x | x <- [1..(1 + 4 * n)] ]

foo [] _ = False
foo (x:xs) n
    | elem (test n x) (p n) = True
    | otherwise = foo xs n

test n x
    | (mod ((1 + 4 * n) / x) == 0) = (1 + 4 * n) / x
    | otherwise = -1

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