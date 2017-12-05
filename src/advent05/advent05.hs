{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}

import qualified Data.IntMap.Strict as M
import Data.IntMap.Strict ((!))
import Control.Monad.State.Lazy

data Machine = Machine { location :: Int
                       , steps :: Int
                       , memory :: M.IntMap Int
                       } deriving (Show, Eq)

main :: IO ()
main = do 
        text <- readFile "data/advent05.txt"
        let locations = map (readJump) $ lines text
        let m0 = makeMachine locations
        print $ evalState stepAll m0
        print $ evalState stepAllB m0


readJump :: String -> Int
readJump = read

makeMachine :: [Int] -> Machine
makeMachine locations = Machine {location = 0, steps = 0,
    memory = M.fromList $ zip [0..] locations}

stepAll :: State Machine Int
stepAll = do
            m0 <- get
            if M.member (location m0) (memory m0)
            then do stepOnce
                    stepAll
            else return (steps m0)

stepAllB :: State Machine Int
stepAllB = do
            m0 <- get
            if M.member (location m0) (memory m0)
            then do stepOnceB
                    stepAllB
            else return (steps m0)

stepOnce :: State Machine ()
stepOnce = 
    do m0 <- get
       let mem = memory m0
       let loc = location m0
       let loc' = mem!loc + loc
       let steps' = steps m0 + 1
       let mem' = M.insert loc (mem!loc + 1) mem
       put m0 {location = loc', steps = steps', memory = mem'}

stepOnceB :: State Machine ()
stepOnceB = 
    do m0 <- get
       let mem = memory m0
       let loc = location m0
       let loc' = mem!loc + loc
       let steps' = steps m0 + 1
       let newVal = if mem!loc >= 3 then (mem!loc - 1) else (mem!loc + 1)
       let mem' = M.insert loc newVal mem
       put m0 {location = loc', steps = steps', memory = mem'} 

