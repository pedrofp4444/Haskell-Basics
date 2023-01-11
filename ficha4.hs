module Ficha4 where 

import Data.Char

digitAlpha :: String -> (String,String)
digitAlpha "" = ("","")
digitAlpha (h:t) | isAlpha h = (h : letras, digitos)
                 | isDigit h = (letras, h : digitos)
                 | otherwise = (letras, digitos)
                where (letras, digitos) = digitAlpha t

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h < 0 = (neg + 1, zero, pos)
          | h > 0 = (neg, zero, pos + 1)
          | otherwise = (neg, zero + 1, pos)
        where (neg, zero, pos) = nzp t
    
divMod' :: Integral a => a -> a -> (a,a)
divMod' x y | x - y < 0 = (0,x)
           | otherwise = (q+1,r)
           where (q,r) = divMod' (x - y) y

fromDigits :: [Int] -> Int
fromDigits l = fromDigitsAux l 0

fromDigitsAux :: [Int] -> Int -> Int
fromDigitsAux [] acc = acc
fromDigitsAux (h:t) acc = fromDigitsAux t (h + 10 * acc)



maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maxSumInitAux l (sum l)

maxSumInitAux :: (Num a, Ord a) => [a] -> a -> a
maxSumInitAux [] acc = acc
maxSumInitAux l acc = if si > acc then maxSumInitAux il si else maxSumInitAux il acc
    where il = init l
          si = sum il

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fibAux (n-2) 0 1

fibAux :: Int -> Int -> Int -> Int
fibAux 0 n _ = n
fibAux i fib_n fib_n_mais_1 = fibAux (i - 1) fib_n_mais_1 (fib_n + fib_n_mais_1)

intToStr :: Integer -> String
intToStr 0 = "zero"
intToStr n = intToStrAux n ""

intToStrAux :: Integer -> String -> String
intToStrAux 0 ('-':acc) = acc
intToStrAux n acc = intToStrAux nn ((case r of 
        0 -> "-zero"
        1 -> "-um"
        2 -> "-dois"
        3 -> "-três"
        4 -> "-quatro"
        5 -> "-cinco"
        6 -> "-seis"
        7 -> "-sete"
        8 -> "-oito"
        9 -> "-nove") ++ acc)
    where (nn,r) = n `divMod` 10


{-
import Data.List

-- [x^2 | x <- [1,2,3,4]
-- [x^2 | x <- [1,2,3,4],even x]
-- [x^2 | x <- [1,2,3,4], x < 3]

-- Exercício 8

-- Alínea a) 

-- [x | x <- [1..20], mod x 2 == 0, mod x 3 == 0]
    --ou
a = [x | x <- [1..20], mod x 6 == 0]

-- Alínea b)

--[x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0]



type Polinomio = [Monomio]
type Monomio = (Int,Int)

grau :: Polinomio -> Int 
grau p = gm
    where (cm, gm) = maximumBy (\(c1,g1) (c2,g2) -> compare g1 g2) p -}