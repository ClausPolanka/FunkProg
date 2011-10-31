module Aufgabe3 ( anp1, anp2, transp ) where

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
    | l == [] = replicate (fromIntegral z) componentListTemplate
    | length l < (fromInteger z) = extended_L
    | otherwise = cropped_L
        where
            componentListTemplate = (replicate (fromIntegral s) w)
            extended_L = customizedComponentLists ++ requiredListsFilledWithValue
            cropped_L = take (fromInteger z) customizedComponentLists
            customizedComponentLists = [cropOrExtendCols x | x <- l]
                where 
                    cropOrExtendCols x
                        | length x < (fromIntegral s) = x ++ replicate ((fromIntegral s) - length x) w
                        | otherwise = take (fromIntegral s) x
            requiredListsFilledWithValue = (replicate ((fromIntegral z) - (length l)) componentListTemplate)

-- Beispiel 3
transp :: [[Integer]] -> Zeilen -> Spalten -> Fuellwert -> Matrix
transp l z s w = anp2 l s z w