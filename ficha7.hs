{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use concatMap" #-}
{-# HLINT ignore "Use record patterns" #-}
module Ficha7 where

data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show, Eq)

data RTree a = R a [RTree a] deriving (Show, Eq)

data LTree a = Tip a | Fork (LTree a) (LTree a) deriving (Show, Eq)

data FTree a b = Leaf b | No a (FTree a b) (FTree a b) deriving (Show, Eq)

arvore_r = R 5 [R 2 [R 1 [], 
                     R 0 [], 
                     R 3 []],
                R 7 [],
                R 9 [R 15 []],
                R 4 [R 6 [],
                     R 0 []]
               ]

arvore_l = Fork (Fork (Fork (Tip 3) (Tip 5)) (Fork (Tip 1) (Tip 8))) (Fork (Fork (Tip 7) (Tip 9)) (Fork (Tip 4) (Tip 6)))

altura :: RTree a -> Int 
altura (R a []) = 1
altura (R a filhos) = maximum (map altura filhos) + 1 

postorder :: RTree a -> [a]
postorder (R a []) = [a]
postorder (R a filhos) = concat (map postorder filhos) ++ [a]

ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork l r) = ltSum l + ltSum r

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf x) = (Empty, Tip x)
splitFTree (No x l r) = (Node x btreel btreer, Fork ltreel ltreer)
    where (btreel, ltreel) = splitFTree l
          (btreer, ltreer) = splitFTree r

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Fork _ _) = Nothing
joinTrees (Node _ _ _) (Tip _) = Nothing
joinTrees Empty (Tip _) = Nothing
joinTrees (Node x lb rb) (Fork ll rl) = 
    case (joinTrees lb ll, joinTrees rb rl)
    of (Nothing, _) -> Nothing
       (_, Nothing) -> Nothing
       (Just full_l, Just full_r) -> Just (No x full_l full_r)
