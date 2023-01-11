-- ATENÇÃO AO IMPORT DA FICHA1

{- É necessário:
    - Colocar o ficheiro da ficha 1 e da 3 na mesma diretoria 
    - Chamar os dois no compilador (ex. ghci ficha1.hs ficha3.hs)
-}

module Ficha3 where

import Ficha1

-- Exercício 1

data Hora = H Int Int deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

-- Alínea a)

etapaBemConstruida :: Etapa -> Bool
etapaBemConstruida (h1,h2) = horaValidar h1 && horaValidar h2 && horaComp h2 h1

-- Alínea b)

viagemBemConstruida :: Viagem -> Bool
viagemBemConstruida [] = True
viagemBemConstruida [a] = etapaBemConstruida a
viagemBemConstruida ((h1,h2):t) = etapaBemConstruida (h1,h2) && viagemBemConstruida t && horaComp h2 h3
    where (h3,h4) = head t

-- Alínea c)

partidaEChegada :: Viagem -> Etapa
partidaEChegada v = H hi hf 
    where (hi,_) = head v
          (_,hf) = last v

{-

-- Alínea d)

tempoViagemEfetiva :: Viagem -> Hora
tempoViagemEfetiva [] = H 0 0
tempoViagemEfetiva ((h1,h2):t) = adicionaHoras (horaDif h2 h1) (tempoDeViagem t)

adicionaHoras :: Hora -> Hora -> Hora
adicionaHoras (H h1 m1) (H h2 m2) = H (h1 + h2 + hExtra) (mod (m1 + mr) 60)
    where hExtra = div (m1 + m2) 60

-- Alínea e)

tempoEspera :: Viagem -> Hora
tempoEspera ((h1,h2):(h3,h4):t) = adicionaHoras (horaDif h3 h2) (tempoDeEspera ((h3,h4):t))
tempoEspera _ = H 0 0

-- Alínea f)

tempoTotalViagem :: Viagem -> Hora
tempoTotalViagem v = adicionaHoras (tempoViagemEfetiva v) (tempoEspera v)


-- Exercício 2 

type Poligonal = [Ponto]

-- Alínea a)

comprimento :: Poligonal -> Double
comprimento (h1:h2:t) = dist h1 h2 + comprimento (h2:t)
comprimento _ = 0

-- Alínea b)

testaFechada :: Poligonal -> Bool
testaFechada p = length p >= 3 && head p == last p

-- Alínea c)

triangula :: Poligonal -> [Figura]
triangula (h1:h2:h3:t) = if h1 == h3
                         then []
                         else Triangulo h1 h2 h3 : triangula (h1:h3:t)
triangula _ = []

-- Alínea d)

area :: Poligonal -> Double
area p = areaTris (triangula p)

areaTris :: [Figura] -> Double
areaTris [] = 0
areaTris (h:t) = area h + areaTris t
-}