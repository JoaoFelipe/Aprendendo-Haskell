module HappyNumbers where

happy :: Int -> String
happy = (`happy'` [])

happy' :: Int -> [Int] -> String
happy' 1 _ = "happy"
happy' x ls 
    | x `elem` ls = "sad"
    | otherwise = happy' (somaDosQuadrados $ algarismos x) (x:ls) 

algarismos :: Int -> [Int]
algarismos y = [(read [x] :: Int) | x <- show y]

somaDosQuadrados :: [Int] -> Int
somaDosQuadrados = sum . map (^ 2)  
