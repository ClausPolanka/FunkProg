module Main where

import HUnit
import Aufgabe2

assureThatZeroIsNotAPrimal = 
    TestCase (assertEqual "Not a primal" False (istPrimal 0))

assureThatOneIsNotAPrimal = 
    TestCase (assertEqual "Not a primal" False (istPrimal 1))

assureThat38IsNotAPrimal = 
    TestCase (assertEqual "Not a primal" False (istPrimal 38))

assureThat173IsNotAPrimal = 
    TestCase (assertEqual "Not a primal" False (istPrimal 173))

allTests = 
    TestList [
        TestLabel " 0 is not a primal" assureThatZeroIsNotAPrimal,
        TestLabel " 2 is not a primal" assureThatOneIsNotAPrimal,
        TestLabel " 38 is not a primal" assureThat38IsNotAPrimal,
        TestLabel " 173 is not a primal" assureThat173IsNotAPrimal
    ]

main = do runTestTT allTests