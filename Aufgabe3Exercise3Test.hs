import HUnit
import Aufgabe3

funkProgAcceptanceTest1 = 
    TestCase (assertEqual "Transponierte Matrix" [[0, 0], 
                                                  [0, 0], 
                                                  [0, 0]] (transp [] 2 3 0))

funkProgAcceptanceTest2 = 
    TestCase (assertEqual "Transponierte Matrix" [[1, 1, 1], 
                                                  [2, 2, 2], 
                                                  [3, 9, 3]] (transp [[1, 2, 3], 
                                                                      [1, 2], 
                                                                      [1, 2, 3, 4, 5], 
                                                                      [1]] 3 3 9))
funkProgAcceptanceTest3 = 
    TestCase (assertEqual "Transponierte Matrix" [[1, 4, (-1)], 
                                                  [2, 5, (-1)], 
                                                  [3, 6, (-1)], 
                                                  [(-1), (-1), (-1)]] (transp [[1, 2, 3],
                                                                               [4, 5, 6]] 3 4 (-1)))

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
        funkProgAcceptanceTest3
    ]
