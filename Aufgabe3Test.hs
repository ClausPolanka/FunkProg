module Main where

import HUnit
import Aufgabe3

emptyList = 
    TestCase (assertEqual "Matrix" [[1]] (anp1 []))

oneComponentListWithOneElement = 
    TestCase (assertEqual "Matrix" [[5]] (anp1 [[5]]))

oneComponentListWithTwoElements = 
    TestCase (assertEqual "Matrix" [[5, 7]] (anp1 [[5, 7]]))

twoComponentListWithOneElement = 
    TestCase (assertEqual "Matrix" [[5], [4]] (anp1 [[5], [4]]))

allTests = 
    TestList [
        TestLabel 
        " For an empty list a component list containing 1 must be returned." 
        emptyList,

        TestLabel 
        " For one component list return the same component list." 
        oneComponentListWithOneElement,

        TestLabel 
        " For one component list return the same component list." 
        oneComponentListWithTwoElements,

        TestLabel 
        " For two component lists both containing one element return the same lists." 
        twoComponentListWithOneElement
    ]

main = do runTestTT allTests