module Aufgabe4 ( msk, mm, ms, mp ) where

import Aufgabe3

type Matrix = [[Integer]]
type Skalar = Integer
type Zeilen = Integer
type Spalten = Integer
type SpaZei = Integer
type Fuellwert = Integer
type ProtoMatrix = ([[Integer]], Zeilen, Spalten, Fuellwert)
type Typung_mnpw = (Zeilen, SpaZei, Spalten, Fuellwert)
type Typung_mnw = (Zeilen, Spalten, Fuellwert)
type Typung_mw = (SpaZei, Fuellwert)
type Potenz = Integer
type ProtoprotoMatrix = [[Integer]]

-- Beispiel 1
msk :: ProtoMatrix -> Skalar -> Matrix
msk (comp_L, z, s, w) sk = [map (*sk) x | x <- (anp2 comp_L z s w)]

-- Beispiel 2
mm :: ProtoprotoMatrix -> ProtoprotoMatrix -> Typung_mnpw -> Matrix
mm p1 p2 (z, sz, s, w) = calculateProductOf m1 m2
    where
        m1 = [x | x <- (anp2 p1 z sz w)]
        m2 = anp2 p2 sz s w

calculateProductOf :: Matrix -> Matrix -> Matrix
calculateProductOf m1 m2 = [productOfAllColumnVectorsWith rowVector m2 | m_i <- [0..(length m1 - 1)], 
                                                                         let rowVector = (m1 !! m_i)]

productOfAllColumnVectorsWith :: [Integer] -> Matrix -> [Integer]
productOfAllColumnVectorsWith rowVector m2 = [productOf rowVector columnVector | p_i <- [0..(p - 1)], 
                                                                                 let columnVector = [x !! p_i | x <- m2]]
    where 
        productOf rV cV = sum (zipWith (*) rV cV)
        p = length (m2 !! 0)

-- Beispiel 3
ms :: ProtoprotoMatrix -> ProtoprotoMatrix -> Typung_mnw -> Matrix
ms p1 p2 (z, s, w) = []

-- Beispiel 4
mp :: ProtoprotoMatrix -> Typung_mw -> Potenz -> Matrix
mp p (sz, w) n = []