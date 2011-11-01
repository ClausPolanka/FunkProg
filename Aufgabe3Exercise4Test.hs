module Main where

import HUnit
import Aufgabe3

funkProgAcceptanceTest1 = 
    TestCase (assertEqual "Skalarprodukt" 9 (sp [[1, 2, 3]] [[4, 5, 6]] 3 1))

funkProgAcceptanceTest2 = 
    TestCase (assertEqual "Skalarprodukt" 32 (sp [[1, 2, 3]] [[4], [5], [6]] 3 1))

allTests = 
    TestList [
        TestLabel 
        " FunkProg Acceptance Test 1." 
        funkProgAcceptanceTest1,

        TestLabel 
        " FunkProg Acceptance Test 2." 
        funkProgAcceptanceTest2
    ]

main = do runTestTT allTests