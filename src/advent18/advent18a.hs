{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

import Control.Monad (when)
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer

import Advent18Parser

data Machine = Machine { registers :: M.Map Char Integer
                       , lastSound :: Integer
                       , pc :: Int
                       } 
               deriving (Show, Eq)

type ProgrammedMachine = WriterT [Integer] (ReaderT [Instruction] (State Machine)) ()

emptyMachine = Machine {registers = M.empty, lastSound = 0, pc = 0}

main :: IO ()
main = do 
        text <- TIO.readFile "data/advent18.txt"
        let instrs = successfulParse text
        let ((result, l), machinef) = part1 instrs
        print $ head l

part1 :: [Instruction] -> (((), [Integer]), Machine)
part1 instructions = 
    runState (
        runReaderT (
            runWriterT executeInstructions
                   ) 
            instructions 
             ) 
             emptyMachine

executeInstructions :: ProgrammedMachine
executeInstructions = 
    do  instrs <- ask
        m <- get
        when (pc m >= 0 && pc m < length instrs)
            $
            do let rt = recoverTriggers instrs m
               if rt
               then tell [lastSound m]
               else do executeInstruction
                       executeInstructions

executeInstruction :: ProgrammedMachine
executeInstruction =
    do  instrs <- ask
        m <- get
        let instr = instrs!!(pc m)
        put (applyInstruction instr m)


isRecover :: Instruction -> Bool
isRecover (Rcv _) = True
isRecover _ = False


recoverTriggers :: [Instruction] -> Machine -> Bool
recoverTriggers instrs m = 
        if isRecover instr
        then (x /= 0)
        else False
        where instr = instrs!!(pc m)
              Rcv a = instr
              x = evaluate m a


applyInstruction :: Instruction -> Machine -> Machine

applyInstruction (Snd sound) m = m {lastSound = freq, pc = pc'}
    where pc' = pc m + 1
          freq = evaluate m sound

applyInstruction (Set (Register a) b) m = m {registers = reg', pc = pc'}
    where pc' = pc m + 1
          y = evaluate m b
          reg' = M.insert a y $ registers m

applyInstruction (Add (Register a) b) m = m {registers = reg', pc = pc'}
    where pc' = pc m + 1
          x = evaluate m (Register a) 
          y = evaluate m b
          reg' = M.insert a (x + y) $ registers m

applyInstruction (Mul (Register a) b) m = m {registers = reg', pc = pc'}
    where pc' = pc m + 1
          x = evaluate m (Register a) 
          y = evaluate m b
          reg' = M.insert a (x * y) $ registers m

applyInstruction (Mod (Register a) b) m = m {registers = reg', pc = pc'}
    where pc' = pc m + 1
          x = evaluate m (Register a) 
          y = evaluate m b
          reg' = M.insert a (x `mod` y) $ registers m

applyInstruction (Rcv _a) m = m {pc = pc'}
    where pc' = pc m + 1
    
applyInstruction (Jgz a b) m = m {pc = pc'}
    where x = evaluate m a
          y = evaluate m b
          pc' = if x > 0 then pc m + (fromIntegral y) else pc m + 1


evaluate :: Machine -> Location -> Integer
evaluate _ (Literal i)  = i
evaluate m (Register r) = M.findWithDefault 0 r (registers m)

