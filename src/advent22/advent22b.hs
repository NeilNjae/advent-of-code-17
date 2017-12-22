{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

import Prelude hiding (Left, Right)
import Data.List
import qualified Data.Map.Strict as M

type Point = (Int, Int)

data Flag = Clean | Weakened | Infected | Flagged deriving (Show, Eq)

type Infection = M.Map Point Flag

data Direction = Up | Right | Down | Left deriving (Show, Eq, Enum)

leftOf Up = Left
leftOf x = pred x

rightOf Left = Up
rightOf x = succ x

delta :: Direction -> Point
delta Up = (-1, 0)
delta Right = (0, 1)
delta Down = (1, 0)
delta Left = (0, -1)

(+:) :: Point -> Point -> Point
(+:) (r, c) (dr, dc) = (r + dr, c + dc)

data World = World { infected :: Infection
                   , position :: Point
                   , direction :: Direction
                   , infectionCount :: Int
                   } deriving (Eq, Show)


main :: IO ()
main = do 
        text <- readFile "data/advent22.txt"
        let grid = lines text
        print $ infectionCount $ progress 10000000 $ initialWorld grid

initialWorld :: [String] -> World
initialWorld grid = World 
    { infected = initialInfected grid
    , position = initialPosition grid
    , direction = Up
    , infectionCount = 0
    }

initialInfected :: [String] -> Infection
initialInfected g = M.fromList [ ((r, c), Infected) 
                               | r <- [0..(length g - 1)]
                               , c <- [0..((length . head) g - 1)]
                               , g!!r!!c == '#'] 

initialPosition :: [String] -> Point
initialPosition g = (length g `div` 2, (length . head) g `div` 2)   


progress :: Int -> World -> World
progress n = (!! n) . iterate step 


step :: World -> World
step world = World { infected = inf', position = pos', direction = dir'
                   , infectionCount = ic'}
    where !here = position world
          !stateHere = M.findWithDefault Clean here (infected world)
          !dir' = case stateHere of 
                      Clean -> leftOf (direction world)
                      Weakened -> direction world
                      Infected -> rightOf (direction world)
                      Flagged -> rightOf (rightOf (direction world))
          !stateHere' = case stateHere of 
                      Clean -> Weakened
                      Weakened -> Infected
                      Infected -> Flagged
                      Flagged -> Clean
          !inf' = M.insert here stateHere' (infected world)          
          !ic'  = if stateHere' == Infected then infectionCount world + 1
                                           else infectionCount world
          !pos' = here +: delta dir'