module Bingo1 where

import System.Random
import Data.List

bingo :: IO ()
bingo = bingoaux [1..10]

bingoaux :: [Int] -> IO()
bingoaux [] = do putStrLn "TAUU"
                 return ()
bingoaux numeros = do putStr "Numero escolhido:"
                      getChar 
                      aleatorio <- randomRIO (0, ((length numeros) - 1))
                      let num = numeros !! aleatorio
                      print num
                      bingoaux (delete num numeros) 