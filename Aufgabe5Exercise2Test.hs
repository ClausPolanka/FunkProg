module Aufgabe5Exercise2Test where

import HUnit
import Aufgabe5

unixtac2Lines =
    TestCase (assertEqual "Reverse order" "Second\nFirst" (unixtac "First\nSecond"))

unixhead3Characters = 
    TestCase (assertEqual "First n characters" "Foo" (unixhead 3 "FooBar"))


allTests = 
    TestList [
        TestLabel 
        " Reverse order for two lines." 
        unixtac2Lines,

        TestLabel 
        " First 3 characters of String." 
        unixhead3Characters
    ]

main = do runTestTT allTests
