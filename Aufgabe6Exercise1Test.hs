module Aufgabe6Exercise1Test where

import HUnit
import Aufgabe6

todo = 
    TestCase (assertEqual "todo" todo (todo))

allTests = 
    TestList [
        TestLabel 
        " todo." 
        todo
    ]

main = do runTestTT allTests