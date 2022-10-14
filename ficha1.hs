module Ficha1 where

-- Exercício 1

-- Alínea a)

perimetro :: Double -> Double
perimetro x = 2 * pi * x

-- Alínea b)

dist :: (Double, Double) -> (Double, Double) -> Double
dist (xa, ya) (xb, yb) = sqrt ((xa - xb) ^ 2 + (ya - yb) ^ 2)

-- Alínea c)

primUlt :: [a] -> (a,a)
primUlt lista = (head lista, last lista)

-- Alínea d)

multiplo :: Int -> Int -> Bool
multiplo m n = mod m n == 0

-- Alínea e)

truncaImpar :: [a] -> [a]
truncaImpar lista = if not (multiplo (length lista) 2)
                    then tail lista
                    else lista

-- Alínea f)

max2 :: Int -> Int -> Int
max2 a b = if a > b
           then a
           else b

-- Alínea g)

max3 :: Int -> Int -> Int -> Int
max3 a b c = if max2 a b == a
            then max2 a c
            else max2 b c

-- Exercício 2

-- Alínea a)

nRaizes :: (Double,Double,Double) -> Int
nRaizes (a,b,c)
    | (b ^ 2 - 4 * a * c) > 0 = 2
    | (b ^ 2 - 4 * a * c) == 0 = 1
    | (b ^ 2 - 4 * a * c) < 0 = 0

-- Alínea b)

raizes :: (Double,Double,Double) -> [Double]
raizes (a,b,c)
    | n == 2 = [(-b + sqrt (b ^ 2 - 4 * a * c))/(2*a),(-b - sqrt (b ^ 2 - 4 * a * c))/(2*a)]
    | n == 1 = [(-b + sqrt (b ^ 2 - 4 * a * c))/(2*a)]
    | n == 0 = []
    where n = nRaizes (a,b,c)

-- Exercício 3

type Hora = (Int,Int)

-- Alínea (a)

horaValidar :: Hora -> Bool
horaValidar (h,m)
            |h < 24 && h >= 0 && m < 60 && m >= 0 = True
            | otherwise = False

-- Alínea (b)

horaComp :: Hora -> Hora -> Bool
horaComp (h1,m1) (h2,m2)
                | h1 > h2 = False
                | h1 == h2 = (m1 < m2)
                | h1 < h2 = True

-- Alínea (c)

horaConvert :: Hora -> Int
horaConvert (h,m) = h * 60 + m

-- Alínea (d)

minConvert :: Int -> Hora 
minConvert m = (div m 60,mod m 60)

-- Alínea (e)

horaDif :: Hora -> Hora -> Int
horaDif (h1,m1) (h2,m2) = if (horaConvert (h1,m1)) > (horaConvert(h2,m2))
                          then horaConvert (h1,m1) - horaConvert (h2,m2)
                          else horaConvert (h2,m2) - horaConvert (h1,m1)

-- Alínea (f)

horaAdd :: Hora -> Int -> Hora
horaAdd (h,m) min = minConvert (horaConvert (h,m) + min)

-- Exercício 5

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

-- Alínea a)

next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

-- Alínea b)

stop :: Semaforo -> Bool
stop Vermelho = True
stop a = False

-- Alínea c)

safe :: Semaforo -> Semaforo -> Bool
safe Vermelho a = True
safe a Vermelho = True
safe a a = False

-- Exercício 6

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

-- Alínea a)

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = r * cos a

-- Alínea b)

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar r a) = r * sin a

-- Alínea c)

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x ^ 2 + y ^ 2)
raio (Polar r a) = r

-- Alínea d)

angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (y/x)
angulo (Polar r a) = a

-- Alínea e)

dist :: Ponto -> Ponto -> Double
dist p1 p2 = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
    where x1 = posx p1
          y1 = posy p1
          x2 = posx p2
          y2 = posy p2

-- Exercício 7

data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

-- Alínea a)

poligono :: Figura -> Bool
poligono (Circulo _ _ ) = False
poligono (Retangulo p1 p2) = posx p1 /= posx p2 && posy p1 /= posy p2
poligono (Triangulo p1 p2 p3) = posx p1 /= posx p2 ||
                                posx p2 /= posx p3 ||
                                posx p1 /= posx p3
                                &&
                                posy p1 /= posy p2 ||
                                posy p2 /= posy p3 ||
                                posy p1 /= posy p3

-- Alínea b)

vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Retangulo p1 p2) = [p1, Cartesiano (posx p1) (posy p2), p2, Cartesiano (posx p2) (posy p1)]
vertices (Triangulo p1 p2 p3) = [p1,p2,p3]

-- Alínea c)

-- Em resolução

-- Alínea d)

perimetro :: Figura -> Double
perimetro (Circulo _ r) = 2 * pi * r
perimetro (Retangulo p1 p2) = abs (posx p2 - posx p1) * 2 + abs (posy p2 - posy p1) * 2
perimetro (Triangulo p1 p2 p3) = dist p1 p2 + dist p2 p3 + dist p1 p3

-- Exercício 8

-- Alínea a)

isLower :: Char -> Bool
isLower c = ord c >= ord 'a' && ord c <= ord 'z'

-- Alínea b) 

isDigit :: Char -> Bool
isDigit d = ord ch >= ord '0' && ord ch <= ord '9'

-- Alínea c)

isAlpha :: Char -> Bool
isAlpha ch = isLower ch || isUpper ch
    where isUpper ch = ord ch >= ord 'A' && ord ch <= ord 'Z'

-- Alínea d)

toUpper :: Char -> Char
toUpper ch = if isLower ch then chr (ord ch - 32) else ch

-- Alínea e)

intToDigit :: Int -> Char
intToDigit n = chr (n + 48)

-- Alínea f)

digitToInt :: Char -> Int 
digitToInt ch = ord ch - 48
