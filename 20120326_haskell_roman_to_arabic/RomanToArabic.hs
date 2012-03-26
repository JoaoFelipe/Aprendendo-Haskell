module RomanToArabic (
    romanToArabic
)where


romanToArabic :: String -> Int
romanToArabic [] = 0
romanToArabic [x] = valueOf x
romanToArabic (x:xs) = 
    (romanToArabic xs) +
    let vx = valueOf x
        vnext = valueOf (head xs)
    in 
        if vx < vnext then -vx else vx
               
        
valueOf :: Char -> Int
valueOf x = case x of 
    'I' -> 1
    'V' -> 5
    'X' -> 10
    'L' -> 50
    'C' -> 100
    'D' -> 500
    'M' -> 1000
    _ -> 0
    

