module Aufgabe6 where

data Point = Point Float Float
data Shape = Rectangle Float Float Float Float | Circle Float Float Float
data Shape' = Rectangle' Point Point | Circle' Point Float
data Car = Car {
    company :: String,
    model :: String,
    year :: Int
} deriving (Show)

surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

surface' :: Shape' -> Float  
surface' (Circle' _ r) = pi * r ^ 2  
surface' (Rectangle' (Point x1 y1) (Point x2 y2) ) = (abs $ x2 - x1) * (abs $ y2 - y1)

createCircles :: Float -> Float -> [Float] -> [Shape]
createCircles x y rs = map (Circle x y) rs 

foo :: String -> String
foo s = s

data DOrd = Infix | Praefix | Postfix | GInfix | GPraefix | GPostfix
data BTree = Nil | BNode Int BTree BTree deriving (Eq, Ord, Show)

-- Beispiel 1
flatten :: BTree -> DOrd -> [Int]
flatten Nil _ = []


-- Beispiel 2
isST :: BTree -> Bool
isST Nil = False

-- Beispiel 3
type Control = String
type Func = Integer -> Integer
type Data = Integer
data Tree = Leaf Func | Node Func Tree Tree Tree

mkControl :: String -> Control
mkControl s = [x | x <- s, x == 'l' || x == 'm' || x == 'r']

apply :: Control -> Data -> Tree -> Integer
apply c d (Leaf f) = f d
apply [] d (Node f t1 t2 t3) = f d
apply c d (Node f t1 t2 t3)
    | (head $ mkControl c) == 'l' = apply (drop 1 c) (f d) t1 
    | (head $ mkControl c) == 'm' = apply (drop 1 c) (f d) t2
    | (head $ mkControl c) == 'r' = apply (drop 1 c) (f d) t3

-- Beispiel 4
data LTree = LNode Integer [LTree] deriving Show

mapLT :: Func -> LTree -> LTree
mapLT f lt = LNode 1 []