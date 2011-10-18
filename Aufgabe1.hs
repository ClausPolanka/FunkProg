-- Aufgabe 1
pick :: Integer -> [Integer] -> [Integer]
pick n l = [x | x <- l, x == n]

-- Aufgabe 2
pickAll :: [Integer] -> [Integer] -> [Integer]
pickAll _ [] = []
pickAll [] _ = []
pickAll l1 (x:xs) = (pick x l1) ++ pickAll l1 xs

-- Aufgabe 3
variations :: Integer -> Integer -> Integer
variations n r
	| n < r || r < 0 = -1
	| otherwise = product (take (fromIntegral r) [n, n-1..0])

-- Aufgabe 4