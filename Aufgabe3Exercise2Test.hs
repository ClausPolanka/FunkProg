module Main where

import HUnit
import Aufgabe3

emptyListWith2RowsAnd3ColumnsFilledUpWithZeros = 
    TestCase (assertEqual "Matrix" [[0, 0, 0], 
                                    [0, 0, 0]] (anp2 [] 2 3 0))

emptyListWith2RowsAnd2ColumnsFilledUpWith5s = 
    TestCase (assertEqual "Matrix" [[5], 
                                    [5]] (anp2 [] 2 1 5))

emptyListOneRow = 
    TestCase (assertEqual "Matrix" [] (anp2 [] 1 0 0))

emptyListOneColumn = 
    TestCase (assertEqual "Matrix" [] (anp2 [] 0 1 0))

emptyListZeroRowsAndColumns = 
    TestCase (assertEqual "Matrix" [] (anp2 [] 0 0 1))

matrix = [[1, 3, 4],
          [2, 7, 8],
          [8, 10, 7]]
componentListsMatchesExactlyRequirements = 
    TestCase (assertEqual "Matrix" matrix (anp2 matrix 3 3 1))

allTests = 
    TestList [
        TestLabel 
        " Empty list with 2 rows and 3 columns filled up with zeros." 
        emptyListWith2RowsAnd3ColumnsFilledUpWithZeros,

        TestLabel 
        " Empty list with 2 rows and 2 columns filled up with 5s." 
        emptyListWith2RowsAnd2ColumnsFilledUpWith5s,

        TestLabel 
        " Empty list with 1 row and 0 columns must return empty list." 
        emptyListOneRow,

        TestLabel 
        " Empty list with 0 rows and 1 column must return empty list." 
        emptyListOneColumn,
        
        TestLabel 
        " Empty list with 0 rows and 0 columns must return empty list." 
        emptyListZeroRowsAndColumns,

        TestLabel 
        " Component lists matches exactly given requirements." 
        componentListsMatchesExactlyRequirements
    ]

main = do runTestTT allTests