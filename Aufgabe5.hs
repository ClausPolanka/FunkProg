module Aufgabe5 where

-- Beispiel 1

ctxlines :: [String]
ctxlines = ["Test", "Test", "Test\nTest\nTest"]

ctxunlines :: [[String]]
ctxunlines = [["Test1\nT"], ["Test2\nT"], ["Test3\nT"], ["T\nT"]]

ctxwords :: [String]
ctxwords = ["Test2 Test3  T", "T  Test1 Test2 Test3", "Test1  T Test2 Test3"]

-- Beispiel 2

unixtac :: String -> String
unixtac s = withoutLastNewLine text
	where text = unlines . reverse . lines $ s

withoutLastNewLine :: String -> String
withoutLastNewLine s = take (length s - 1) s 

unixhead :: Int -> String -> String
unixhead n text = withoutLastNewLine nLines
	where nLines = unlines . take n  $ lines text

unixtail :: Int -> String -> String
unixtail n text = withoutLastNewLine nLines
	where 
		nLines = unlines . drop (length linesAsList - n) $ linesAsList
		linesAsList = lines text

unixgrep :: String -> String -> String
unixgrep s1 s2 = s1

-- Beispiel 3

aslines :: ([String]->[String]) -> String -> String
aslines f s = s

unixtac' :: String -> String
unixtac' s = s

unixhead' :: Int -> String -> String
unixhead' n s = s

unixtail' :: Int -> String -> String
unixtail' n s = s

unixgrep' :: String -> String -> String
unixgrep' s1 s2 = s1

-- Beispiel 4

unixrev :: String -> String
unixrev s = s

wordrev :: String -> String
wordrev s = s

-- Beispiel 5

unixwcw :: String -> Int
unixwcw s = 1

unixwc :: String -> (Int, Int, Int)
unixwc s = (1, 1, 1)

