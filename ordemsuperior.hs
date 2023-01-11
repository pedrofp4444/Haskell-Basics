-- | Velocidade que irá afetar a movimentação dos 'Obstaculo's de um 'Mapa'.
type Velocidade = Int

{- | Cada linha de um 'Mapa' é um Terreno em particular contendo 'Obstaculo's.

As linhas do tipo 'Rio' ou 'Estrada' têm a propriedade 'Velocidade' que indicam a velocidade de deslocamento dos obstáculos. Podendo esta ser negativa, indicando assim a sua direção.
-}
data Terreno
  = Rio Velocidade
  | Estrada Velocidade
  | Relva
  deriving (Show, Read, Eq)

-- | Um Obstáculo numa linha de um 'Mapa'.
data Obstaculo
  = Nenhum -- ^ a ausência de obstáculos
  | Tronco -- ^ os troncos deslizam apenas em 'Rio'
  | Carro -- ^ os carros movimentam-se apenas em 'Estrada'
  | Arvore -- ^ as árvores são um obstáculo fixo que não se move e apenas são possíveis em 'Relva'
  deriving (Show, Read, Eq)

-- | Comprimento de um 'Mapa'.
type Largura = Int

-- | O Mapa que constituí o 'Jogo'.
data Mapa =
  Mapa Largura [(Terreno, [Obstaculo])]
  deriving (Show, Read, Eq)


proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos largura (_, obstaculos) | largura == (length obstaculos) = []
proximosObstaculosValidos largura (Estrada _, _) = [Nenhum, Carro]
proximosObstaculosValidos largura (Rio _, _) = [Nenhum, Tronco]
proximosObstaculosValidos largura (Relva, _) = [Nenhum, Arvore]

mapa2 = (Mapa 5 [(Rio 1, [Tronco, Nenhum, Tronco, Tronco, Tronco]),
                 (Rio (-1), [Nenhum, Nenhum, Tronco, Tronco, Tronco]),
                 (Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Nenhum]),  -- Suposto dar False (Mais do que 4 rios seguidos)
                 (Rio (-2), [Nenhum, Nenhum, Nenhum, Tronco, Nenhum]),
                 (Rio 2, [Nenhum, Tronco, Tronco, Nenhum, Nenhum])
                ])








mapaEx = (Mapa 10 [(Rio 2, [Nenhum, Tronco, Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco, Nenhum]),(Rio (-2), [Nenhum, Tronco, Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco, Nenhum]),(Rio 2, [Nenhum, Tronco, Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco, Nenhum]),(Rio (-2), [Nenhum, Tronco, Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco, Nenhum]) ])

















{-
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa mapa@(Mapa l (par@(ter, obst):ts)) seed | terreno == Relva = Mapa l ((Relva, obstaculos):par:ts)
                                                    | terreno == Rio 0 = Mapa l ((Rio v, obstaculos):par:ts)
                                                    | terreno == Estrada 0 = Mapa l ((Estrada v, obstaculos):par:ts)
    where
        terrenos = proximosTerrenosValidos mapa
        terreno = terrenos !! (mod seed (length terrenos))
        obstaculos = proximosObstaculosValidos l (terreno, [])
        obstaculo = obstaculos !! (mod seed (length obstaculos))
        v = (delete 0 [-l..l]) !! (mod seed (2*l+1))
-}