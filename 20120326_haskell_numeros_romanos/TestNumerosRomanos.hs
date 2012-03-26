module Main where

import Test.HUnit
import System.Exit
import Control.Monad
import NumerosRomanos


main = do counts <- runTestTT tests
          when ((errors counts, failures counts) /= (0, 0)) $
              exitWith (ExitFailure 1)

tests = TestList [numerosRomanosTest]

numerosRomanosTest = TestList [
        "toRomanNumber 1 retorna I" ~: 
            "I" ~=? toRomanNumber 1,
            
        "toRomanNumber 2 retorna II" ~:
            "II" ~=? toRomanNumber 2,
            
        "toRomanNumber 3 retorna III" ~:
            "III" ~=? toRomanNumber 3,
            
        "toRomanNumber 5 retorna V" ~:
            "V" ~=? toRomanNumber 5,
        
        "toRomanNumber 4 retorna IV" ~:
            "IV" ~=? toRomanNumber 4,    
            
        "toRomanNumber 6 retorna VI" ~:
            "VI" ~=? toRomanNumber 6,   
            
        "toRomanNumber 7 retorna VII" ~:
            "VII" ~=? toRomanNumber 7,   
            
        "toRomanNumber 8 retorna VIII" ~:
            "VIII" ~=? toRomanNumber 8,
            
        "toRomanNumber 9 retorna IX" ~:
            "IX" ~=? toRomanNumber 9,
            
        "toRomanNumber 10 retorna X" ~:
            "X" ~=? toRomanNumber 10,
            
        "toRomanNumber 11 retorna XI" ~:
            "XI" ~=? toRomanNumber 11,
        
        "toRomanNumber 30 retorna XXX" ~:
            "XXX" ~=? toRomanNumber 30,
        
        "toRomanNumber 34 retorna XXXIV" ~:
            "XXXIV" ~=? toRomanNumber 34,
        
        "toRomanNumber 50 retorna L" ~:
            "L" ~=? toRomanNumber 50,
        
        "toRomanNumber 40 retorna XL" ~:
            "XL" ~=? toRomanNumber 40,
        
        "toRomanNumber 41 retorna XLI" ~:
            "XLI" ~=? toRomanNumber 41,
        
        "toRomanNumber 99 retorna XCIX" ~:
            "XCIX" ~=? toRomanNumber 99,
        
        "toRomanNumber 100 retorna C" ~:
            "C" ~=? toRomanNumber 100,
            
        "toRomanNumber 500 retorna D" ~:
            "D" ~=? toRomanNumber 500,
        
        "toRomanNumber 888 retorna DCCCLXXXVIII" ~:
            "DCCCLXXXVIII" ~=? toRomanNumber 888,
        
        "toRomanNumber 1000 retorna M" ~:
            "M" ~=? toRomanNumber 1000,
        
        "toRomanNumber 3888 retorna MMMDCCCLXXXVIII" ~:
            "MMMDCCCLXXXVIII" ~=? toRomanNumber 3888,
        
        "toRomanNumber 3999 retorna MMMCMXCIX" ~:
            "MMMCMXCIX" ~=? toRomanNumber 3999
            
    ]

