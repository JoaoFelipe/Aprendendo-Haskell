module Boliche where

data CharNum = Special Char | Score Int | Last Int

boliche :: [(CharNum, CharNum)] -> Int
boliche [] = 0
boliche ((Score a, Score b):cs) = a + b + boliche cs
boliche ((Score a, Special '/'):cs) = (spare cs) + boliche cs
boliche ((Special 'X', Special '-'):cs) = (strike cs) + boliche cs
boliche ((Special a, Special 'X'):cs) = (strike cs) + 10 + boliche cs
boliche ((Special a, Score b):cs) = (strike cs) + b + boliche cs
boliche _ = 0

spare :: [(CharNum, CharNum)] -> Int
spare [] = 10
spare ((Score a, b):cs) = 10 + a
spare ((Special a, b):cs) = 20 

strike :: [(CharNum, CharNum)] -> Int
strike [] = 10
strike ((Score a, Score b):cs) = 10 + a + b
strike ((Score a, Special b):cs) = 20
strike ((Special 'X', Special '-'):(Special 'X', Special '-'):cs) = 30 
strike ((Special 'X', Special '-'):(Score c, d):cs) = 20 + c
strike ((Special 'X', Special '-'):cs) = 20
strike ((Special 'X', b):cs) = 30  
strike ((Score a, Last b):cs) = 10 + a 

score :: (Int, Int) -> (CharNum, CharNum)
score (a,b) = (Score a, Score b)

constStrike :: (CharNum, CharNum)
constStrike = (Special 'X', Special '-')

constSpare :: Int -> (CharNum, CharNum)
constSpare a = (Score a, Special '/')

lastScore :: CharNum -> CharNum -> CharNum -> [(CharNum, CharNum)]
lastScore a b c = [(a,b), (c, Last 0)]

