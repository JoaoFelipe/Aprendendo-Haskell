module NumerosRomanos where

toRomanNumber :: Integral a => a -> String
toRomanNumber x
    | 1 <= x && x < 10 = roman 'I' 'V' 'X' x
    | 10 <= x && x < 100 = (roman 'X' 'L' 'C' (x `div` 10)) ++ toRomanNumber (x `mod` 10)
    | 100 <= x && x < 1000 = (roman 'C' 'D' 'M' (x `div` 100)) ++ toRomanNumber (x `mod` 100)
    | 1000 <= x && x < 4000 = (roman 'M' '-' '-' (x `div` 1000)) ++ toRomanNumber (x `mod` 1000)
    | otherwise = ""
    
    
roman :: Integral a => Char -> Char -> Char -> a -> String    
roman i v x number 
    | number == 4 = [i, v]
    | number == 5 = [v]
    | number == 9 = [i, x]
    | 1 <= number && number <= 3 = number `times` i
    | 6 <= number && number <= 9 = [v] ++ ((number - 5) `times` i)
    | otherwise = ""  
    where times number letra = take (fromIntegral number) (repeat letra)
    
    
