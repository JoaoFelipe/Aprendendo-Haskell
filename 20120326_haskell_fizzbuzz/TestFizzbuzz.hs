module Main where

import Test.HUnit
import System.Exit
import Control.Monad
import Fizzbuzz


main = do counts <- runTestTT tests
          when ((errors counts, failures counts) /= (0, 0)) $
              exitWith (ExitFailure 1)

tests = TestList [fizzbuzzTest]

fizzbuzzTest = TestList [
        "fizzbuzz 1 retorna 1" ~: 
            "1" ~=? fizzbuzz 1,
            
        "fizzbuzz 2 retorna 2" ~:
            "2" ~=? fizzbuzz 2,
            
        "fizzbuzz 3 retorna fizz" ~:
            "fizz" ~=? fizzbuzz 3,
            
        "fizzbuzz 6 retorna fizz" ~:
            "fizz" ~=? fizzbuzz 6,
            
        "fizzbuzz 5 retorna buzz" ~:
            "buzz" ~=? fizzbuzz 5, 
             
        "fizzbuzz 10 retorna buzz" ~:
            "buzz" ~=? fizzbuzz 10,  
            
        "fizzbuzz 15 retorna fizzbuzz" ~:
            "fizzbuzz" ~=? fizzbuzz 15,
            
        "fizzbuzz 30 retorna fizzbuzz" ~:
            "fizzbuzz" ~=? fizzbuzz 30
    ]

