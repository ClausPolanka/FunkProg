module Main where

import HUnit
import Aufgabe3

emptyList = 
    TestCase (assertEqual "Transponierte Matrix" [[0, 0], 
                                                  [0, 0], 
                                                  [0, 0]] (transp [] 2 3 0))

allTests = 
    TestList [
        TestLabel 
        " Fill empty list with required component lists." 
        emptyList
    ]

main = do runTestTT allTests