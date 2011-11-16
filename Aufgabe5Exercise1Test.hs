module Aufgabe5Exercise1Test where

import HUnit
import Aufgabe5

todo = 
    TestCase (assertEqual "TODO" <TODO> (TODO))    

allTests = 
    TestList [
        TestLabel 
        " TODO." 
        todo
    ]

main = do runTestTT allTests