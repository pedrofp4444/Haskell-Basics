module UltimaAula where

data Ponto = P Float Float

instance Eq Ponto where
    P x1 y1 == P x2 y2 = x1 == x2 && y1 == y2

instance Ord Ponto where
    P x1 y1 <= P x2 y2 = sqrt (x1^2 + y1^2) <= sqrt (x2^2 + y2^2)

instance Show Ponto where
    show(P x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Num Ponto where
    P x1 y1 + P x2 y2 = P (x1 + x2) (y1 + y2)
    P x1 y1 * P x2 y2 = P (x1 * x2) (y1 * y2)

data Genero = M | F | NB deriving (Show)

data Pessoa = Pessoa {
    nome :: String,
    idade :: Int,
    cc :: Integer,
    genero :: Genero 
}

instance Eq Pessoa where
    Pessoa n1 i1 cc1 genero1 == Pessoa n2 i2 cc2 genero2 = cc1 == cc2 

instance Ord Pessoa where
    Pessoa n1 i1 cc1 genero1 <= Pessoa n2 i2 cc2 genero2 = i1 <= i1

instance Show Pessoa where
    show (Pessoa n i cc genero) = "Nome: " ++ show n ++ "\n" ++ "Idade: " ++ show i ++ "\n" ++ "CC: " ++ show cc ++ "\n" ++ "Genero: " ++ show genero


-- type Idade = Int 
-- type CC = Intenger

-- data Pessoa = Nome Idade CC Genero 
