module Ficha2 where

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

funC (x:y:t) = funC tail
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
        | otherwise = notas t

-- Alínea (g)

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((_,h2):t) = h2 : segundos t

-- Exercício 4

type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- Alínea (a)

conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n ((coificiente,grau):t)
        | grau == n = 1 + conta n t 
        | otherwise = conta n t 

-- Alínea (d)

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c,g):t) = (c * fromIntegral g, g - 1): deriv t

-- Alínea (g)

mult :: Monomio -> Polinomio -> Polinomio
mult n [] = []
mult n (h:t) = multM n h : mult n t
    where multM :: Monomio -> Monomio -> Monomio
          multM (a,b) (b,d) = (a * c, b + d)

-- Alínea (h)

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((c,g):t) =  inserir (c,g) (normaliza t) 

inserir :: Monomio -> Polinomio -> Polinomio
inserir m [] = [m]
inserir (cm,gm) ((c,g):t) 
        | g == gm = (c+cm,g) : t
        | otherwise = (c,g) : inserir (cm,gm) t

--