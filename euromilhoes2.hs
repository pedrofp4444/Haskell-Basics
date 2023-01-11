module Euromilhoes2 where

import Data.List

data Aposta = Ap [Int] (Int,Int)

valida :: Aposta -> Bool
valida (Ap numeros@[num1,num2,num3,num4,num5] (estrela1, estrela2)) = all (`elem` [1..50]) numeros && numeros == nub numeros && elem estrela1 [1..9] && elem estrela2 [1..9] && estrela1 /= estrela2
valida _ = False

comuns :: Aposta -> Aposta -> (Int,Int)
comuns (Ap numeros1 estrelas1) (Ap numeros2 estrelas2) = (length (intersect numeros1 numeros2), length (filter (`elem` [fst estrelas2, snd estrelas2]) [fst estrelas1, snd estrelas1]))

instance Eq Aposta where
    (==) :: Aposta -> Aposta -> Bool
    (==) a b = comuns a b == (5,2)

premio :: Aposta -> Aposta -> Maybe Int
premio aposta solucao =
    case comuns aposta solucao of 
        (5,e) -> Just (3 - e)
        (4,e) -> Just (6 - e)
        (3,2) -> Just 7
        (3,e) -> Just (10 - e)
        (2,2) -> Just 8
        (2,e) -> Just (13 - e)
        (1,2) -> Just 11
        _ -> Nothing

leAposta :: IO Aposta
leAposta = do
    putStrLn "Introduz 5 números separados por um espaço:"
    nums <- map read . words <$> getLine :: IO [Int]
    putStrLn "Introduz as 2 estrelas separadas por um espaço:"
    estrelas <- map read . words <$> getLine :: IO [Int]
    if length estrelas /= 2 then
        putStrLn "Aposta invalida!"
        >> leAposta    
    else
        let ap = Ap nums ((\(a:b:_) -> (a,b)) estrelas) in
        if valida ap then
            return ap
        else
            putStrLn "Aposta invalida!"
            >> leAposta

joga :: Aposta -> IO ()
joga chave = do aposta <- leAposta
                let recompensa = premio aposta chave
                print recompensa

geraChave :: IO Aposta
geraChave = undefined