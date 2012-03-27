module Caminhada where
import Data.List

caminhada :: Integral a => [[a]] -> [a]
caminhada = caminhada' . transpose

caminhada' :: Integral a => [[a]] -> [a]
caminhada' x = --(caminhos x) !! 0
    let cs = caminhos x
        vs = [valor x c | c <- cs]
        menor = minimum vs
        menorI = findIndex (== menor) vs
    in cs !! (extractJust menorI) 
    
extractJust :: Maybe Int -> Int
extractJust Nothing = 0
extractJust (Just x) = x
    
valor :: Integral a => [[a]] -> [a] -> a
valor [] _ = 0
valor _ [] = 0
valor (l:ls) (x:xs) = (l !! (fromIntegral x)) + (valor ls xs)

caminhos :: Integral a => [[a]] -> [[a]]
caminhos = caminhos' (-1)
     
caminhos' :: Integral a => a -> [[a]] -> [[a]]     
caminhos' anterior (x:[]) = [[y] | y <- proximos anterior x]
caminhos' anterior (x:xs) = concat [map ([y] ++) (caminhos' y xs) | y <- proximos anterior x]

proximos :: Integral b => b -> [a] -> [b]
proximos (-1) ls = enumerate ls
proximos x ls = filter (\y -> x - 1 <= y && y <= x + 1) (enumerate ls) 
--para problema Unidirectional TSP:
--proximos x ls = filter (\y -> (x - 1 <= y && y <= x + 1) || (x == length (ls - 1) && y = 0) || (x == 0 && y == length (ls - 1))) (enumerate ls) 

enumerate :: Integral b => [a] -> [b]
enumerate x = take (length x) [0..]

