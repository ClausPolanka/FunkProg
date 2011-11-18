module Aufgabe5Exercise2Test where

import HUnit
import Aufgabe5

unixtac2Lines =
    TestCase (assertEqual "Reverse order" "Second\nFirst" (unixtac "First\nSecond"))

unixhead3Characters = 
    TestCase (assertEqual "First n lines" "Foo\nBar\nFooBaar" (unixhead 3 "Foo\nBar\nFooBaar\nNOT"))

unixtail3Characters = 
    TestCase (assertEqual "Last n lines" "Foo\nBar\nFooBaar" (unixtail 3 "NOT\nFoo\nBar\nFooBaar"))

unixgrepLinesContainingSearchString = 
    TestCase (assertEqual "Lines" "Mein Name ist Claus Polanka\nXXXxxxPolanakXXX" (unixgrep "ol" "NOT\nMein Name ist Claus Polanka\nBar\nFooBaar\nXXXxxxPolanakXXX"))

findInSentence =
    TestCase (assertEqual "Found" True (contains "Polanka" "an"))


allTests = 
    TestList [
        TestLabel 
        " Reverse order for two lines." 
        unixtac2Lines,

        TestLabel 
        " First 3 lines of text." 
        unixhead3Characters,

        TestLabel 
        " Last 3 lines of text." 
        unixtail3Characters,

        TestLabel 
        " 'an' must be found in 'Polanka'." 
        findInSentence,

        TestLabel
        " Line containing 'ol' must be shown."
        unixgrepLinesContainingSearchString
    ]

main = do runTestTT allTests
