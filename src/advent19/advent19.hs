{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Prelude hiding (Left, Right)
import Data.List
import Data.Char

type Maze = [String]

data Direction = Up | Down | Left | Right deriving (Show, Eq)

data Progress = Progress { row :: Int
                         , column :: Int
                         , direction :: Direction
                         , letters :: String
                         , stepCount :: Int
                         } deriving (Show, Eq)


-- Note: assumes the maze comes with a padding border of spaces
-- all around it. Makes the "next location" checking much easier!

main :: IO ()
main = do 
        text <- readFile "data/advent19.txt"
        let maze = lines text
        let progress = navigate maze
        print $ letters progress
        print $ stepCount progress


startProgress :: Maze -> Progress
startProgress maze = Progress { row = 0, column = startCol
                              , direction = Down
                              , letters = "", stepCount = 0}
    where topRow = maze!!0
          startCol = head $ elemIndices '|' topRow  

delta :: Direction -> (Int, Int)
delta Up    = (-1,  0)
delta Down  = ( 1,  0)
delta Left  = ( 0, -1)
delta Right = ( 0,  1)

isJunction '+' = True
isJunction  _  = False 

isFinished :: Maze -> Progress -> Bool
isFinished maze progress = isSpace $ location maze (row progress) (column progress)

location :: Maze -> Int -> Int -> Char
location maze r c = (maze!!r)!!c


navigate :: Maze -> Progress
navigate maze = navigate' maze progress
    where progress = startProgress maze

navigate' :: Maze -> Progress -> Progress
navigate' maze progress = 
    if isFinished maze progress 
        then progress
        else navigate' maze (step maze progress)


step :: Maze -> Progress -> Progress
step maze progress = progress {row = r', column = c', direction = d', letters = l', stepCount = sc'}
    where r = row progress
          c = column progress
          thisChar = location maze r c
          l' = if isAlpha thisChar then (letters progress) ++ [thisChar] else letters progress
          d' = if isJunction thisChar then newDirection maze progress else direction progress 
          (dr, dc) = delta d'
          r' = r + dr
          c' = c + dc
          sc' = stepCount progress + 1

newDirection :: Maze -> Progress -> Direction
newDirection maze progress = 
    if d == Up || d == Down 
    then if isSpace leftChar then Right else Left
    else if isSpace upChar then Down else Up
    where d = direction progress
          r = row progress
          c = column progress
          upChar = location maze (r - 1) c
          leftChar = location maze r (c - 1)
