module Bolao where
import Data.List
import Data.Function

type Resultado = (Int, Int)


pontuacao :: Resultado -> Resultado -> Int
pontuacao (a,b) (c,d) 
    | a == c && b == d = 4
    | a > b && c > d = 2
    | b > a && d > c = 2
    | a == b && c == d = 2
    | b == d = 1
    | a == c = 1
    | otherwise = 0


data Aposta = Aposta { 
    nome :: String,
    apostas :: [Resultado]
}

bolao :: [Aposta] ->  [Resultado] -> [(String,Int)]
bolao apostas resultados = reverse $ sortBy (compare `on` snd) [(n, total as resultados) | (Aposta n as) <- apostas]


total :: [Resultado] -> [Resultado] -> Int
total _ [] = 0
total [] _ = 0
total (a:as) (c:cs) = (pontuacao a c) + (total as cs)
