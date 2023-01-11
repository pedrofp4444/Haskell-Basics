module EuroMilhoes where

import Data.List
import Data.Foldable

data Aposta = Ap [Int] (Int,Int)

valida :: Aposta -> Bool
valida (Ap numeros@[n1,n2,n3,n4,n5] (e1,e2)) = all (`elem` [1..50]) numeros && numeros == nub numeros && elem e1 [1..12] && elem e2 [1..12] && e1 /= e2
valida _ = False 

comuns :: Aposta -> Aposta -> (Int,Int)
comuns ap1@(Ap numeros1 e1) ap2@(Ap numeros2 e2) =  (length (numeros1 `intersect` numeros2), length (filter (`elem` [fst e2, snd e2]) [fst e1, snd e2]))

instance Eq Aposta where
    (==) :: Aposta -> Aposta -> Bool
    (==) a b = comuns a b == (5,2)

premio :: Aposta -> Aposta -> Maybe Int
premio ap chave =
    case comuns ap chave of 
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


lerNome :: IO()
lerNome = do
            putStrLn "Insere o teu nome:"
            nome <- getLine
            print nome