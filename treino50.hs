module Treino50 where 

enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 a b 
        | a < b = a : enumFromTo1 (a + 1) b       
        | a > b = a : enumFromTo1 (a - 1) b
        | a == b = [a]  

enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 a b c 
            | a < b && a <= c = a : enumFromThenTo1 b (b + (b - a)) c
            | a > b && a >= c = a : enumFromThenTo1 b (b - (a - b)) c
            | otherwise = []

-- (++)
cola :: [a] -> [a] -> [a]
cola [] b = b
cola (h:t) b = h : cola t b

-- (!!)
f :: [a] -> Int -> a
f [] n = error "O elemento nao existe na lista"
f (h:t) n 
        | n == 0 = h
        | otherwise = f t (n - 1)

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (h:t) = (reverse t) ++ [h]

take1 :: Int -> [a] -> [a]
take1 n [] = []
take1 0 _ = []
take1 n l@(h:t)
            | n >= (length l) = l
            | otherwise = h : take1 (n - 1) t

drop1 :: Int -> [a] -> [a]
drop1 0 l = l
drop1 _ [] = []
drop1 n l@(h:t)
        | n >= (length l) = []
        | otherwise = drop1 (n - 1) t

replicate1 :: Int -> a -> [a]
replicate1 n x 
        | n <= 0 = []
        | otherwise = x : replicate1 (n - 1) x


sufixo :: Eq a => [a] -> [a] -> Bool
sufixo [] _ = True
sufixo _ [] = False
sufixo l1 l2 = (last l1 == last l2) || sufixo (init l1) (init l2) 

sufixo2 :: Eq a => [a] -> [a] -> Bool
sufixo2 [] _ = True
sufixo2 _ [] = False
sufixo2 l (h:t) = l == (h:t) || sufixo2 l t

lookup1 :: Eq a => a -> [(a,b)] -> Maybe b
lookup1 _ [] = Nothing
lookup1 x ((a,b):t) = if x == a
                     then Just b
                     else lookup1 x t

heads :: [[a]] -> [a]
heads [] = []
heads ([]:t) = heads t
heads (h:t) = (head h) : heads t

fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):t) = [(a,c)] ++ fun t

prefixo :: Eq a => [a] -> [a] -> Bool
prefixo _ [] = False
prefixo [] _ = True
prefixo (h1:t1) (h2:t2)
        | h1 == h2 = prefixo t1 t2
        |otherwise = False

sufixo3 :: Eq a => [a] -> [a] -> Bool
sufixo3 _ [] = False
sufixo3 [] _ = True
sufixo3 l1 l2
        | last l1 == last l2 = sufixo3 (init l1) (init l2)
        |otherwise = False