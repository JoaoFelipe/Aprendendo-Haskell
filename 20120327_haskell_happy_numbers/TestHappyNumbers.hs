module Main where

import Test.HUnit
import System.Exit
import Control.Monad
import HappyNumbers


main = do counts <- runTestTT tests
          when ((errors counts, failures counts) /= (0, 0)) $
              exitWith (ExitFailure 1)

tests = TestList [happyNumbersTest]

happyNumbersTest = TestList [
        "1 é feliz" ~: 
            "happy" ~=? happy 1
             
        ,"2 é triste" ~: 
            "sad" ~=? happy 2
            
        ,"10 é feliz" ~: 
            "happy" ~=? happy 10
          
        ,"100 é feliz" ~: 
            "happy" ~=? happy 100
            
        ,"7 é feliz" ~: 
            "happy" ~=? happy 7 
            
        ,"3 é triste" ~: 
            "sad" ~=? happy 3  
        
        ,"5 é triste" ~: 
            "sad" ~=? happy 5
            
        ,"376 é feliz" ~: 
            "happy" ~=? happy 376
            
        ,"1024 é triste" ~: 
            "sad" ~=? happy 1024
        
    ]

