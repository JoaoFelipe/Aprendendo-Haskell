module Fizzbuzz (
    fizzbuzz
) where

fizzbuzz :: Integral a => a -> String
fizzbuzz x 
    | sayFizzBuzz x = "fizzbuzz"
    | sayFizz x = "fizz"
    | sayBuzz x = "buzz"
    | otherwise = show x
    where 
        sayFizz = (== 0) . (`mod` 3)
        sayBuzz = (== 0) . (`mod` 5)
        sayFizzBuzz x = all ($ x) [sayFizz, sayBuzz]
