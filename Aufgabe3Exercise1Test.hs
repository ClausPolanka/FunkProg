module Main where

import HUnit
import Aufgabe3

emptyList = 
    TestCase (assertEqual "Matrix" [[1]] (anp1 []))

oneComponentListWithTwoElements = 
    TestCase (assertEqual "Matrix" [[5, 7]] (anp1 [[5, 7]]))

threeComponentListsWithOneElement = 
    TestCase (assertEqual "Matrix" [[5], [4], [1]] (anp1 [[5], [4], [1]]))

threeComponentLists_1Has1Element_1Has2Elements_1Has3Elements = 
    TestCase (assertEqual "Matrix" [[5, 0, 0], [4, 2, 0], [1, 3, 7]] (anp1 [[5], [4, 2], [1, 3, 7]]))

allTests = 
    TestList [
        TestLabel 
        " For an empty list a component list containing 1 must be returned." 
        emptyList,

        TestLabel 
        " For 1 component list return the same component list." 
        oneComponentListWithTwoElements,

        TestLabel
        " For 3 component lists all containing one element return the same lists."
        threeComponentListsWithOneElement,

        TestLabel
        "For 3 component lists 1 has 1, 1 has 2 and 1 has 3 elements add necessary 0s to all lists."
        threeComponentLists_1Has1Element_1Has2Elements_1Has3Elements
    ]

main = do runTestTT allTests