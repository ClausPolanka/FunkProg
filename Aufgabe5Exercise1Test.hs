module Aufgabe5Exercise1Test where

import HUnit
import Aufgabe5

linesWith1Line = TestCase (assertEqual "Lines" 1 (length . lines $ "Claus"))
linesWith2Line = TestCase (assertEqual "Lines" 2 (length . lines $ "Claus\nPolanka"))
wordsWith2Words = TestCase (assertEqual "Lines" 2 (length . words $ "Claus Polanka"))
wordsWith3Words = TestCase (assertEqual "Lines" 3 (length . words $ "Claus\nPolanka Babs"))

unlinesLines = 
    TestCase (assertEqual "Not equal" True (all (\x->(unlines.lines)x/=x) ctxlines))

lengthLowerThan2 = 
    TestCase (assertEqual "Not equal" True ((((<)2).length) ctxlines))

anyStringLenghtLowerThan2 =
    TestCase (assertEqual "Not equal" True (any (((<)2).length.lines) ctxlines))

allTests = 
    TestList [
        TestLabel " Reading 1 word in 1 line, length of list must be 1." linesWith1Line,
        TestLabel " Reading 1 word in 2 lines, length of list must be 2." linesWith2Line,
        TestLabel " Reading 2 words, length of list must be 2." wordsWith2Words,
        TestLabel " Reading 3 words, length of list must be 3." wordsWith3Words,

        TestLabel " After calling unlines and lines result ist not the same." unlinesLines,
        TestLabel " Length must be lower than 2." anyStringLenghtLowerThan2
    ]

main = do runTestTT allTests