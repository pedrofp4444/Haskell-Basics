module Bingo where
import System.Random
import Data.List

bingo :: IO()
bingo = bingoAux [1..10]

bingoAux :: [Int] -> IO()
bingoAux [] = return ()
bingoAux prev_ns = do
                    putStr "Prime ENTER para gerar novo numero"
                    getChar
                    random_index <- randomRIO (1, length prev_ns)
                    let n = prev_ns !! (random_index - 1)
                    print n 
                    bingoAux (delete n prev_ns)