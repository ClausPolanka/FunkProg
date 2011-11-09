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
mm p1 p2 (z, sz, s, w) = []

-- Beispiel 3
ms :: ProtoprotoMatrix -> ProtoprotoMatrix -> Typung_mnw -> Matrix
ms p1 p2 (z, s, w) = []

-- Beispiel 4
mp :: ProtoprotoMatrix -> Typung_mw -> Potenz -> Matrix
mp p (sz, w) n = []