module Main where

import Test.HUnit
import System.Exit
import Control.Monad
import Boliche


main = do counts <- runTestTT tests
          when ((errors counts, failures counts) /= (0, 0)) $
              exitWith (ExitFailure 1)

tests = TestList [bolicheTest]

bolicheTest = TestList [
        "boliche [(0,0)] retorna 0" ~: 
            0 ~=? boliche [score (0,0)]
             
        ,"boliche [(1,0)] retorna 1" ~: 
            1 ~=? boliche [score (1,0)] 
            
        ,"boliche [(1,1)] retorna 2" ~: 
            2 ~=? boliche [score (1,1)] 
            
        ,"boliche [(0,0),(1,0)] retorna 1" ~: 
            1 ~=? boliche [score (0,0), score (1,0)]   
            
        ,"boliche [(1,0),(1,0)] retorna 2" ~: 
            2 ~=? boliche [score (1,0), score (1,0)] 
            
        ,"boliche [(3,'/')] retorna 10" ~: 
            10 ~=? boliche [constSpare 3] 
            
        ,"boliche [('X','-')] retorna 10" ~: 
            10 ~=? boliche [constStrike] 
            
        ,"boliche [(3,'/'),(3,5)] retorna 21" ~: 
            21 ~=? boliche [constSpare 3, score (3,5)]
            
        ,"boliche [(3,'/'),('X','-')] retorna 30" ~: 
            30 ~=? boliche [constSpare 3, constStrike]
            
        ,"boliche [(3,'/'),('9','/')] retorna 29" ~: 
            29 ~=? boliche [constSpare 3, constSpare 9]    
            
        ,"boliche [('X','-'),('X','-')] retorna 30" ~: 
            30 ~=? boliche [constStrike, constStrike]
            
        ,"boliche [('X','-'),('X','-'), ('X', '-')] retorna 60" ~: 
            60 ~=? boliche [constStrike, constStrike, constStrike]        
            
        ,"boliche [('X','-'),('X','-'), (3, '/')] retorna 53" ~: 
            53 ~=? boliche [constStrike, constStrike, constSpare 3]   
            
        ,"boliche [(7,'/'),(4,2)] retorna 20" ~: 
            20 ~=? boliche [constSpare 7, score (4,2)] 
           
        ,"boliche [('X','-'),('X','-'), ('X', '-'), (0,9)] retorna 78" ~: 
            78 ~=? boliche [constStrike, constStrike, constStrike, score (0,9)] 
            
        ,"boliche [('X','-'),('X','-'), (9,0)] retorna 57" ~: 
            57 ~=? boliche [constStrike, constStrike, score (9,0)]
         
        ,"boliche lastscore (9,/,1) retorna 11" ~: 
            11 ~=? boliche (lastScore (Score 9) (Special '/') (Score 1))
        
        ,"boliche lastscore (9,/,X) retorna 20" ~: 
            20 ~=? boliche (lastScore (Score 9) (Special '/') (Special 'X'))
        
        ,"boliche lastscore (X,1,1) retorna 12" ~: 
            12 ~=? boliche (lastScore (Special 'X') (Score 1) (Score 1))
            
        ,"boliche lastscore (X,X,0) retorna 20" ~: 
            20 ~=? boliche (lastScore (Special 'X') (Special 'X') (Score 0))
            
        ,"boliche [X, -] lastscore (X,X,0) retorna 50" ~: 
            50 ~=? boliche ([constStrike] ++ (lastScore (Special 'X') (Special 'X') (Score 0)))
            
        ,"boliche [3, /] lastscore (X,X,0) retorna 40" ~: 
            40 ~=? boliche ([constSpare 3] ++ (lastScore (Special 'X') (Special 'X') (Score 0)))    
        ,"boliche [(X, -),(X, -),(X, -),(X, -)] retorna 90" ~: 
            90 ~=? boliche (take 4 (repeat constStrike))
            
        ,"boliche 9 strikes + lastScore X X X retorna 300" ~:
            300 ~=? boliche ((take 9 (repeat constStrike)) ++ (lastScore (Special 'X') (Special 'X') (Special 'X')))   
            
        ,"boliche [X,-] [X,-] [X,-] [7,2] [8,/] [0,9] [X,-] [7,/] [9,-] retorna 152" ~:
            152 ~=? boliche ([constStrike, constStrike, constStrike, score (7,2), constSpare 8, score (0,9), constStrike, constSpare 7, score (9,0)]) 
            
        ,"boliche [X,-] [X,-] [X,-] [7,2] [8,/] [0,9] [X,-] [7,/] [9,0] [X,X,8] retorna 180" ~:
            180 ~=? boliche ([constStrike, constStrike, constStrike, score (7,2), constSpare 8, score (0,9), constStrike, constSpare 7, score (9,0)] ++ (lastScore (Special 'X') (Special 'X') (Score 8)))  
            
            
        ,"boliche [X,-] [6,0] [9,0] [8,1] [9,/] [6,/] [3,6] [8,0] [8,1] [9,0,0] retorna 104" ~:
            104 ~=? boliche ([constStrike, score (6,0), score (9,0),score (8,1),constSpare 9, constSpare 6, score (3,6), score (8,0), score (8,1)] ++ (lastScore (Score 9) (Score 0) (Score 0)))        
            
        ,"boliche [8,0] [3,5] [6,2] [9,0] [7,/] [6,3] [7,/] [8,0] [0,9] [8,/,6] retorna 109" ~:
            109 ~=? boliche ([score (8,0), score (3,5), score (6,2),score (9,0),constSpare 7, score (6,3), constSpare 7, score (8,0), score (0,9)] ++ (lastScore (Score 8) (Special '/') (Score 6)))      
            
        ,"boliche [9,0] [4,2] [0,7] [3,0] [9,0] [8,1] [1,0] [1,6] [8,0] [6,/,X] retorna 79" ~:
            79 ~=? boliche ([score (9,0), score (4,2), score (0,7),score (3,0),score (9,0), score (8,1), score (1,0), score (1,6), score (8,0)] ++ (lastScore (Score 6) (Special '/') (Special 'X')))          
            
            
                                         
                                
    ]

