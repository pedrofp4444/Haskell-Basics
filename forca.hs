module Main where
import System.Random



main = do
    palavras <- readFile $ "palavras.txt" >>= (return . filter ((>= 5). length) lines)
    n <- randomRIO (1, length palavras)
    let palavra = palavras !! (n - 1)
    return palavra 

ciclo :: String -> [Char] -> IO()
ciclo palavra letras 