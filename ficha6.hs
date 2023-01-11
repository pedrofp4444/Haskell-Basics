module Ficha6 where

-- Exercício 1 

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

arvore = Node 5 (Node 2 (Node 1 Empty
                                Empty)
                        (Node 3 Empty
                                Empty))
                (Node 9 (Node 7 (Node 6 Empty
                                        Empty)
                                (Node 8 Empty
                                        Empty))
                        Empty)

-- Alínea a) 

altura :: BTree a -> Int
altura Empty = 0
altura (Node e l r) = 1 + max (altura l) (altura r)

-- Alínea b)

contaNodos :: BTree a -> Int 
contaNodos Empty = 0
contaNodos (Node e l r) = 1 + contaNodos l + contaNodos r

-- Alínea c)

folhas :: BTree a -> Int
folhas Empty = 0 
folhas (Node e Empty Empty) = 1 
folhas (Node e l r) = folhas l + folhas r 

-- Alínea d)

prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty
prune n Empty = Empty
prune n (Node e l r) = (Node e cortal cortar)
        where cortal = prune (n - 1) l 
              cortar = prune (n - 1) r

-- Alínea e)




mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node e l r) = (Node e (mirror r) (mirror l))

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty _ = Empty
zipWithBT f _ Empty = Empty
zipWithBT f (Node e1 l1 r1) (Node e2 l2 r2) = (Node (f e1 e2) (zipWithBT f l1 l2) (zipWithBT f r1 r2))



minimo :: Ord a => BTree a -> a
minimo Empty = error "arvore vazia"
minimo (Node e Empty _) = e
minimo (Node e l _) = minimo l

semMinimo :: Ord a => BTree a -> BTree a
semMinimo Empty = error "arvore vazia"
semMinimo (Node e Empty _) = Empty
semMinimo (Node e l r) = (Node e (semMinimo l) r)

minSmin :: Ord a => BTree a -> (a, BTree a)
minSmin Empty = error "arvore vazia"
minSmin (Node e Empty r) = (e,r)
minSmin (Node e l r) = (r_minimo, Node e r_semMinimo r)
        where (r_minimo, r_semMinimo) = minSmin l