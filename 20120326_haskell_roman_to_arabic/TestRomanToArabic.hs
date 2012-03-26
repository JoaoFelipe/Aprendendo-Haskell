module Main where

import Test.HUnit
import System.Exit
import Control.Monad
import RomanToArabic


main = do counts <- runTestTT tests
          when ((errors counts, failures counts) /= (0, 0)) $
              exitWith (ExitFailure 1)

tests = TestList [romanToArabicTest]

romanToArabicTest = TestList [
        "romanToArabic I retorna 1" ~: 
             1 ~=? romanToArabic "I",
             
        "romanToArabic II retorna 2" ~: 
             2 ~=? romanToArabic "II",
             
        "romanToArabic III retorna 3" ~: 
             3 ~=? romanToArabic "III",
            
        "romanToArabic V retorna 5" ~: 
             5 ~=? romanToArabic "V",
            
        "romanToArabic IV retorna 4" ~: 
             4 ~=? romanToArabic "IV",
             
        "romanToArabic VI retorna 6" ~: 
             6 ~=? romanToArabic "VI",
             
        "romanToArabic IX retorna 9" ~: 
             9 ~=? romanToArabic "IX",
             
        "romanToArabic X retorna 10" ~: 
             10 ~=? romanToArabic "X",
             
        "romanToArabic XXXIX retorna 39" ~: 
             39 ~=? romanToArabic "XXXIX",
             
        "romanToArabic XL retorna 40" ~: 
             40 ~=? romanToArabic "XL",
             
        "romanToArabic L retorna 50" ~: 
             50 ~=? romanToArabic "L",
             
        "romanToArabic C retorna 100" ~: 
             100 ~=? romanToArabic "C",
             
        "romanToArabic D retorna 500" ~: 
             500 ~=? romanToArabic "D",
             
        "romanToArabic M retorna 1000" ~: 
             1000 ~=? romanToArabic "M",
             
        "romanToArabic MMMDCCCLXXXVIII retorna 3888" ~: 
             3888 ~=? romanToArabic "MMMDCCCLXXXVIII"
            
    ]

