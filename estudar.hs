module Estudar where

import Data.List

preCrescente :: Ord a => [a] -> [a]
preCrescente [x] = [x]
preCrescente (a:b:t) | a < b = a : preCrescente (b:t)
                     | otherwise = [a]

amplitude :: [Int] -> Int
amplitude [] = 0
amplitude l@(h:t) = amplitudeAux l h h

amplitudeAux :: [Int] -> Int -> Int -> Int
amplitudeAux [] mini maxi = maxi - mini 
amplitudeAux (h:t) mini maxi
                    | h <= mini = amplitudeAux t h maxi
                    | h >= maxi = amplitudeAux t mini h 
                    | otherwise = amplitudeAux t mini maxi

type Mat a = [[a]]

soma :: Num a => Mat a -> Mat a -> Mat a
soma [] _ = []
soma _ [] = []
soma (h1:t1) (h2:t2) = somaLinha h1 h2 : soma t1 t2  

somaLinha :: Num a => [a] -> [a] -> [a]
somaLinha [] _ = []
somaLinha _ [] = []
somaLinha (h1:t1) (h2:t2) = h1 + h2 : somaLinha t1 t2

matriz = [ [1,2], [3,4] ]

replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n x = x : replicate1 (n - 1) x 

intersect1 :: Eq a => [a] -> [a] -> [a]
intersect1 [] _ = []
intersect1 (h:t) l 
        | elem h l = h : intersect1 t l 
        | otherwise = intersect1 t l