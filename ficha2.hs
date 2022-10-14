module Ficha2 where

import Data.Char 

-- Exercício 1

-- Alínea (a)

funA :: [Double] -> Double
funA [] = 0
funA (h:t) = h^2 + (funA t)

-- Alínea (b)

funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2) == 0
             then h: (funB t)
             else (funB t)

-- Alínea (c)

funC :: [Int] -> [Int]
funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []

-- Exercício (d)

funD l = g [] l
g acc [] = acc
g acc (h:t) = g (h:acc) t

-- Exercício 2

-- Alínea (a)

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = h * 2 : dobros t

-- Alínea (b)

numOcorre :: Char -> String -> Int
numOcorre c [] = 0
numOcorre c (h:t) 
            | c == h = 1 + numOcorre c t
            | otherwise = numOcorre c t

-- Alínea (c)

positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t)
    | h > 0 = positivos t
    | otherwise = False

-- Alínea (d)

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t)
        | h > 0 = h : soPos t
        | otherwise = soPos t

-- Alínea (e)

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t)
    | h < 0 = h + somaNeg t
    | otherwise = somaNeg t

-- Alínea (f)

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t)
    | length t < 3 = h:t
    | otherwise = tresUlt t

-- Alínea (g)

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((_,h2):t) = h2 : segundos t

-- Alínea (h)

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros x ((h1,_):t) = x == h1 || nosPrimeiros x t

-- Alínea (i)

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):t) = (a+ra, b+rb, c+rc)
    where (ra,rb,rc) = sumTriplos t

-- Exercício 3

-- Alínea (a)

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t)
    | isDigit h = h : soDigitos t
    | otherwise = soDigitos t

-- Alínea (b)

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t)
    | isLower h = 1 + minusculas t
    | otherwise = minusculas t

-- Alínea (c) (ATENÇÃO)

{-Usei funções pré-definidas para facilitar o exercício-}

nums :: String -> [Int]
nums [] = []
nums (h:t)
    | isDigit h = digitToInt h : nums t
    | otherwise = nums t

-- Exercício 4

type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- Alínea (a)

conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n ((coificiente,grau):t)
        | grau == n = 1 + conta n t 
        | otherwise = conta n t 

-- Alínea (b)

grau :: Polinomio -> Int
grau [] = 0
grau ((c,g):t)
    | g > grau t = g
    | otherwise = grau t

-- Alínea (c)

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau n ((c,g):t)
    | n == g = (c,g) : selgrau n t
    | otherwise = selgrau n t

-- Alínea (d)

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c,g):t) = (c * fromIntegral g, g - 1): deriv t

-- Alínea (e)

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula x ((c,g):t) = c * (x ^ g) + calcula x t

-- Alínea (f)

simp :: Polinomio -> Polinomio
simp [] = []
simp ((c,g):t)
    | c == 0 = simp t
    | otherwise = (c,g) : simp t

-- Alínea (g)

mult :: Monomio -> Polinomio -> Polinomio
mult n [] = []
mult n (h:t) = multM n h : mult n t
    where multM :: Monomio -> Monomio -> Monomio
          multM (a,b) (c,d) = (a * c, b + d)

-- Alínea (h)

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((c,g):t) =  inserir (c,g) (normaliza t) 

inserir :: Monomio -> Polinomio -> Polinomio
inserir m [] = [m]
inserir (cm,gm) ((c,g):t) 
        | g == gm = (c+cm,g) : t
        | otherwise = (c,g) : inserir (cm,gm) t

-- Alínea (i) (ATENÇÃO)

{-Cuidado com a normalização dos polinómios-}

soma :: Polinomio -> Polinomio -> Polinomio
soma x [] = x
soma [] x = x
soma ((c,g):t) x = soma2 (c,g) (soma t x)
    where
        soma2 :: Monomio -> Polinomio -> Polinomio
        soma2 m [] = [m]
        soma2 (c2,g2) ((c,g):t) = if g2 == g
                                  then (c2 + c,g) : t
                                  else (c,g) : soma2 (c2,g2) t

-- Alínea (j)

produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (m:t) p = (mult m p) ++ produto t p

-- Alínea (k)

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (m:t) = inserir m (ordena t)
    where
        inserir :: Monomio -> Polinomio -> Polinomio
        inserir m [] = [m]
        inserir (c2,g2) ((c,g):t)
            | g > g2 = (c2,g2) : (c,g) : t
            | otherwise = (c,g) : inserir (c2,g2) t

-- Alínea (l)

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = corrigir (simplifica p1) == corrigir (simplifica p2)
        where 
            corrigir :: Polinomio -> Polinomio
            corrigir [] = []
            corrigir (m:t) = acrescentar m (corrigir t)
                where
                    acrescentar :: Monomio -> Polinomio -> Polinomio
                    acrescentar m [] = [m]
                    acrescentar (c2,g2) ((c,g):t)
                        | g > g2 = (c2,g2) : (c,g) : t
                        | otherwise = (c,g) : acrescentar (c2,g2) t
            simplifica :: Polinomio -> Polinomio
            simplifica [] = []
            simplifica ((c,g):t) =  juntar (c,g) (simplifica t) 
                where
                    juntar :: Monomio -> Polinomio -> Polinomio
                    juntar m [] = [m]
                    juntar (cm,gm) ((c,g):t) 
                            | g == gm = (c+cm,g) : t
                            | otherwise = (c,g) : juntar (cm,gm) t
                            
