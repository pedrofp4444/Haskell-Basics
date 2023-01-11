module Mastermind where

import System.Random
import Data.Traversable
import Data.List (replicate)


mastermind :: IO ()
mastermind = do sequence (replicate 4 $ randomRIO (0,9))
    >>= playMastermind

playMastermind :: [Int] -> IO ()
playMastermind numbers = do
    guess <- map read . words <$> getLine :: IO [Int]
    if length guess /= 4 then
        putStrLn "Sequencia invalida!"
        >> playMastermind numbers
    else
        let (right_loc, wrong_loc) = 
                foldr (\i (right_loc, wrong_loc) -> 
                    if guess !! i == numbers !! i then
                        (right_loc + 1, wrong_loc)
                    else if guess !! i `elem` numbers then
                        (right_loc, wrong_loc + 1)
                    else
                        (right_loc, wrong_loc)
                ) (0,0) [0..3] in
        if right_loc == 4 then
            putStrLn "Parabens! Acertaste na sequencia!"
        else
            putStrLn (unlines [
                "Valores corretos: " ++ show right_loc,
                "Valores no local errado: " ++ show wrong_loc
            ])
            >> playMastermind numbers