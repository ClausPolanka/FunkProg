module Main where

import HUnit
import Aufgabe3

funkProgAcceptanceTest1 = 
    TestCase (assertEqual "Skalarprodukt" 9 (sp [[1, 2, 3]] [[4, 5, 6]] 3 1))

funkProgAcceptanceTest2 = 
    TestCase (assertEqual "Skalarprodukt" 32 (sp [[1, 2, 3]] [[4], [5], [6]] 3 1))

funkProgAcceptanceTest3 = 
    TestCase (assertEqual "Skalarprodukt" 10 (sp [[1, 2, 3]] [[4, 5, 6]] 4 1))


funkProgAcceptanceTest4 = 
    TestCase (assertEqual "Skalarprodukt" 33 (sp [[1, 2, 3]] [[4], [5], [6]] 4 1))

funkProgAcceptanceTest5 = 
    TestCase (assertEqual "Skalarprodukt" 34 (sp [[1, 2, 3], [6, 6, 7, 8], [3, 45]] [[4, 5, 6], [1, 2]] 4 4))

funkProgAcceptanceTest6 = 
    TestCase (assertEqual "Skalarprodukt" 35 (sp [] [[2, 3, 4]] 2 5))

allTests = 
    TestList [
        TestLabel 
        " FunkProg Acceptance Test 1." 
        funkProgAcceptanceTest1,

        TestLabel 
        " FunkProg Acceptance Test 2." 
        funkProgAcceptanceTest2,

        TestLabel 
        " FunkProg Acceptance Test 3." 
        funkProgAcceptanceTest3,

        TestLabel 
        " FunkProg Acceptance Test 4." 
        funkProgAcceptanceTest4,

        TestLabel 
        " FunkProg Acceptance Test 5." 
        funkProgAcceptanceTest5,

        TestLabel 
        " FunkProg Acceptance Test 6." 
        funkProgAcceptanceTest6
    ]

main = do runTestTT allTests