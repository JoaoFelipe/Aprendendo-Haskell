module Main where

import Test.HUnit
import System.Exit
import Control.Monad
import Bolao


main = do counts <- runTestTT tests
          when ((errors counts, failures counts) /= (0, 0)) $
              exitWith (ExitFailure 1)

tests = TestList [pontuacaoTest, bolaoTest]

pontuacaoTest = TestList [
        "deve retornar 0 quando toda aposta estiver errada" ~: 
            0 ~=? pontuacao (0, 0) (2, 1)
             
        ,"deve retornar 1 quando acertar gols do time B" ~:
            1 ~=? pontuacao (0, 1) (2, 1)
            
        ,"deve retornar 1 quando acertar gols do time A" ~:
            1 ~=? pontuacao (2, 0) (2, 3)
            
        ,"deve retornar 2 quando acertar empate" ~:
            2 ~=? pontuacao (0, 0) (1, 1) 
            
        ,"deve retornar 2 quando acertar time A ganha" ~:
            2 ~=? pontuacao (1, 0) (2, 1)        
            
        ,"deve retornar 2 quando acertar time B ganha" ~:
            2 ~=? pontuacao (1, 2) (2, 3)       
            
        ,"deve retornar 4 quando acertar pontuacao" ~:
            4 ~=? pontuacao (1, 2) (1, 2)                
    ]
    
bolaoTest = TestList [
        "'Joao' não acerta nada'" ~:
            let aposta1 = Aposta "Joao" [(0,0)]
                resultados = [(2,1)]
                apostas = [aposta1]
            in 
                [("Joao", 0)] ~=? bolao apostas resultados
                
        ,"'Joao' acerta um numero de gols'" ~:
            let aposta1 = Aposta "Joao" [(0,1)]
                resultados = [(2,1)]
                apostas = [aposta1]
            in 
                [("Joao", 1)] ~=? bolao apostas resultados
                
        ,"'Joao' acerta uma vitoria e um resultado'" ~:
            let aposta1 = Aposta "Joao" [(1,0), (1,0)]
                resultados = [(2,1), (1,0)]
                apostas = [aposta1]
            in 
                [("Joao", 6)] ~=? bolao apostas resultados      
                
        ,"'Joao' acerta uma vitoria e 'Mario' um resultado'" ~:
            let aposta1 = Aposta "Joao" [(1,0), (0,1)]
                aposta2 = Aposta "Mario" [(0,0), (1,0)]
                resultados = [(2,1), (1,0)]
                apostas = [aposta2, aposta1]
            in 
                [("Mario", 4), ("Joao", 2)] ~=? bolao apostas resultados  
                
        ,"'Joao' acerta uma vitoria e 'Mario' um resultado' - ordenar" ~:
            let aposta1 = Aposta "Joao" [(1,0), (0,1)]
                aposta2 = Aposta "Mario" [(0,0), (1,0)]
                resultados = [(2,1), (1,0)]
                apostas = [aposta1, aposta2]
            in 
                [("Mario", 4), ("Joao", 2)] ~=? bolao apostas resultados
                
        ,"'Joao' acerta uma partidad e erra a outra, 'Joao' acerta estado de 2 partidas, 'Ana' erra 3 partidas" ~:
            let aposta1 = Aposta "Joao" [(3,2), (3,2), (3,2)]
                aposta2 = Aposta "Mario" [(1,0), (0,1), (3,2)]
                aposta3 = Aposta "Ana" [(0,1), (0,1), (3,2)]
                resultados = [(1,0), (1,0), (0,0)]
                apostas = [aposta1, aposta2, aposta3]
            in 
                [("Mario", 4), ("Joao", 4), ("Ana", 0)] ~=? bolao apostas resultados

    ]    


