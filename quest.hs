module Questoes where

-- Exercício 1

enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 a b 
            | a < b = a : enumFromTo1 (a+1) b 
            | a > b = a : enumFromTo1 (a-1) b   
            | a == b = [a]

-- Exercício 2

enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 a b c
                | a < b && a < c = a : enumFromThenTo1 b (b+(b-a)) c 
                | a > b && a > c = a : enumFromThenTo1 b (b-(a-b)) c 
                | otherwise = []

-- Exercício 3

concatenar :: [a] -> [a] -> [a]
concatenar [] x = x
concatenar (h:t) l = h : concatenar t l

-- Exercício 4

localizarelem :: [a] -> Int -> a
localizarelem (h:t) b
    | b == 0 = h
    | b /= 0 = localizarelem t (b-1)

-- Exercício 5

reverse1 :: [a] -> [a]
reverse1 l
        | length l == 1 || length l == 0 = l
        | otherwise = last l : reverse1 (init l)

-- Exercício 6

takep :: Int -> [a] -> [a]
takep a (h:t)
    | a == 0 = []
takep a (h:t) = h : takep (a-1) t

-- Exercício 7 (ATENÇÃO)

{-Quando o a é negativo devolve a lista segundo a função pré-definida-}

drop1 :: Int -> [a] -> [a]
drop1 a [] = [] 
drop1 a (h:t)
        | a <= 0 = (h:t)
        | a > 0 = drop (a-1) t

-- Exercício 8

zip1 :: [a] -> [b] -> [(a,b)]
zip1 [] _ = []
zip1 _ [] = []
zip1 (x:xs) (h:t) = (x,h) : zip1 xs t 

-- Exercício 9

replicate1 :: Int -> a -> [a]
replicate1 b a 
        | b <= 0 = []
        | otherwise = a : replicate1 (b-1) a

-- Exercício 10

intersperse1 :: a -> [a] -> [a]
intersperse1 _ [] = []
intersperse1 _ [x] = [x]
intersperse1 a (h:t) = h : a : intersperse1 a t

-- Exercício 11

group1 :: Eq a => [a] -> [[a]]
group1 [] = [[]]
group1 (h:t) = group2 [h] t
        where
        group2 :: Eq a => [a] -> [a] -> [[a]]
        group2 l [] = [l]
        group2 l (h:t) | elem h l = group2 (h:l) t
                       | otherwise = l : group2 [h] t

-- Exercício 12 

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (h:t) = h ++ concat1 t

-- Exercício 13

inits1 :: [a] -> [[a]]
inits1 [] = [[]]
inits1 l = inits1 (init l) ++ [l] 

-- Exercício 14

tails1 :: [a] -> [[a]]
tails1 [] = []
tails1 (h:[]) = [[h],[]]
tails1 (h:t) = [(h:t)] ++ tails1 t

-- Exercício 15

heads1 :: [[a]] -> [a]
heads1 [] = []
heads1 ([]:t) = heads1 t 
heads1 (h:t) = head h : heads1 t

-- Exercício 16

total1 :: [[a]] -> Int
total1 [] = 0
total1 (h:t) = length h + total1 t


