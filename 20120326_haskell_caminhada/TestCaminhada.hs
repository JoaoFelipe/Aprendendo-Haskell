module Main where

import Test.HUnit
import System.Exit
import Control.Monad
import Caminhada


main = do counts <- runTestTT tests
          when ((errors counts, failures counts) /= (0, 0)) $
              exitWith (ExitFailure 1)

tests = TestList [caminhadaTest, caminhoTest, valorTest]


valorTest = TestList [
        "valor [0] em [[1]] deve retornar 1" ~:
            let mapa = [[1]]
                caminho = [0]
                resultado = 1
            in resultado ~=? valor mapa caminho
            
        ,"valor [0] em [[2]] deve retornar 2" ~:
            let mapa = [[2]]
                caminho = [0]
                resultado = 2
            in resultado ~=? valor mapa caminho
            
        ,"valor [0,0] em [[1,2],[3,4]] deve retornar 4" ~:
            let mapa = [[1,2],
                        [3,4]]
                caminho = [0,0]
                resultado = 4
            in resultado ~=? valor mapa caminho 
            
        ,"valor [0,1] em [[1,2],[3,4]] deve retornar 5" ~:
            let mapa = [[1,2],
                        [3,4]]
                caminho = [0,1]
                resultado = 5
            in resultado ~=? valor mapa caminho        
        
        ,"valor [1,0] em [[1,2],[3,4]] deve retornar 5" ~:
            let mapa = [[1,2],
                        [3,4]]
                caminho = [1,0]
                resultado = 5
            in resultado ~=? valor mapa caminho  
        
        ,"valor [1,1] em [[1,2],[3,4]] deve retornar 6" ~:
            let mapa = [[1,2],
                        [3,4]]
                caminho = [1,1]
                resultado = 6
            in resultado ~=? valor mapa caminho  
            
    ]

caminhoTest = TestList [
        "caminhos [[1]] retorna [[0]]" ~:
            let 
                resultado = [[0]]
                
                entrada = [[1]]
            in 
                resultado ~=? caminhos entrada
            
        ,"caminhos [[1],[2]] retorna [[0,0]]" ~:
            let 
                resultado = [[0,0]]
                
                entrada = [[1],[2]]
            in 
                resultado ~=? caminhos entrada
        
        , "caminhos [[1],[1,2]] retorna [[0,0], [0,1]]" ~:
            let 
                resultado = [[0,0], 
                             [0,1]]
                
                entrada = [[1],[1,2]]
            in 
                resultado ~=? caminhos entrada
            
        ,"caminhos [[1,1],[1,1]] retorna [[0,0],[0,1],[1,0],[1,1]]" ~:
            let 
                resultado = [[0,0],[0,1],
                             [1,0],[1,1]]
                
                entrada = [[1,1],
                           [1,1]]
            in 
                resultado ~=? caminhos entrada
        
        ,"caminhos [[1,2,3],[4,5,6],[7,8,9]] deve considerar apenas proximidades" ~:
            let 
                resultado = [[0,0,0],[0,0,1],
                             [0,1,0],[0,1,1],[0,1,2],
                             [1,0,0],[1,0,1],
                             [1,1,0],[1,1,1],[1,1,2],
                             [1,2,1],[1,2,2],
                             [2,1,0],[2,1,1],[2,1,2],
                             [2,2,1],[2,2,2]]
                             
                entrada = [[1,2,3],
                           [4,5,6],
                           [7,8,9]]
            in 
                resultado ~=? caminhos entrada
    ]

caminhadaTest = TestList [

        "caminhada [[3]] retorna [0]" ~: 
            [0] ~=? caminhada [[3]]
             
        ,"caminhada [[1]] retorna [0]" ~:
            [0] ~=? caminhada [[1]] 
            
        ,"caminhada [[1,2],[3,3]] retorna [0,0]" ~:
            [0,0] ~=? caminhada [[1,2],
                                 [3,3]]
                                 
        ,"caminhada [[1,3],[3,2]] retorna [0,1]" ~:
            [0,1] ~=? caminhada [[1,3],
                                 [3,2]]                         
                                 
        ,"caminhada [[3,3],[1,2]] retorna [1,1]" ~:
            [1,1] ~=? caminhada [[3,3],
                                 [1,2]]     
                                 
        ,"caminhada [[1,3,4],[2,3,4],[3,6,7]] retorna [0,0,0]" ~:
            [0,0,0] ~=? caminhada [[1,3,4],
                                   [2,3,4],
                                   [3,6,7]]                          
        
        ,"caminhada [[1,3,5],[2,3,4],[6,3,7]] retorna [0,0,1]" ~:
            [0,0,1] ~=? caminhada [[1,3,5],
                                   [2,3,4],
                                   [6,3,7]]                          
            
    ]

