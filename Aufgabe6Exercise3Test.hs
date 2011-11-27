module Aufgabe6Exercise3Test where

import HUnit
import Aufgabe6

mkControlSimple = 
    TestCase (assertEqual "Control" "lmr" (mkControl "abcldefmxyzrabc"))

mkControlAdvanced = 
    TestCase (assertEqual "Control" "rrllmmrr" (mkControl "rrxxlabcldmefmxyzrabrc"))

allTests = 
    TestList [
        TestLabel 
        " Given a random string, delete all characters different from l, m and r." 
        mkControlSimple,

        TestLabel 
        " Given an advanced random string, delete all characters different from l, m and r." 
        mkControlAdvanced
    ]

main = do runTestTT allTests