module Ficha5 where 

import Data.List

any' :: (a -> Bool) -> [a] -> Bool
any' funcao [] = False
any' funcao (h:t) = funcao h || any' funcao t

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs
zipWith' _ _ _ = []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (h:t) 
    | f h = h : takeWhile' f t
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (h:t) 
    | f h = dropWhile' f t
    | otherwise = h:t
