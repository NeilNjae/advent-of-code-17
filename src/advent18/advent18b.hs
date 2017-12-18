{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

import Control.Monad (when, unless)
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer

import Advent18Parser


data Machine = Machine { registers :: M.Map Char Integer
                       , pc :: Int
                       , messageQueue :: [Integer]
                       } 
               deriving (Show, Eq)

data MachinePair = MachinePair { machine0 :: Machine 
                               , machine1 :: Machine 
                               } deriving (Show, Eq)

type ProgrammedMachinePair = WriterT [String] (ReaderT [Instruction] (State MachinePair)) ()


emptyMachine = Machine {registers = M.empty, messageQueue = [], pc = 0}

emptyMachinePair = MachinePair { machine0 = emptyMachine {registers = M.singleton 'p' 0}
                               , machine1 = emptyMachine {registers = M.singleton 'p' 1}
                               }

main :: IO ()
main = do 
        text <- TIO.readFile "data/advent18.txt"
        let instrs = successfulParse text
        let ((result, l), statef) = part2 instrs
        print $ length l

part2 :: [Instruction] -> (((), [String]), MachinePair)
part2 instructions = 
    runState (
        runReaderT (
            runWriterT executeInstructions
                   ) 
            instructions 
             ) 
             emptyMachinePair

executeInstructions :: ProgrammedMachinePair
executeInstructions = 
    do  instrs <- ask
        mp <- get
        let m0 = machine0 mp
        let m1 = machine1 mp
        let instr0 = instrs !! pc m0
        let m0Blocked = isReceive instr0 && null (messageQueue m0)
        let instr1 = instrs !! pc m1
        let m1Blocked = isReceive instr1 && null (messageQueue m1)
        let (ma, mb) = if m0Blocked then (m1, m0) else (m0, m1)
          
        unless (m0Blocked && m1Blocked)
            $
            when (pc ma >= 0 && pc ma < length instrs)
                $
                do let m0Active = not m0Blocked
                   when (m0Blocked && isSend instr1) (tell ["sending: " ++ show mp])
                   executeInstruction m0Active
                   executeInstructions

executeInstruction :: Bool -> ProgrammedMachinePair
executeInstruction m0Active =
    do  instrs <- ask
        mp <- get
        let (ma, mb) = if m0Active 
                       then (machine0 mp, machine1 mp) 
                       else (machine1 mp, machine0 mp)
        let mq = messageQueue mb
        let instr = instrs!!(pc ma)
        let (ma', mq') = applyInstruction instr mq ma
        let mb' = mb {messageQueue = mq'}
        let mp' = if m0Active then mp {machine0 = ma', machine1 = mb'}
                              else mp {machine0 = mb', machine1 = ma'}
        put mp'
applyInstruction :: Instruction -> [Integer] -> Machine -> (Machine, [Integer])

-- applyInstruction (Snd a) other m = (m {registers = reg', pc = pc'}, other ++ [y])
--     where pc' = pc m + 1
--           y = evaluate m a
--           sentCount = evaluate m (Register 'x')
--           reg' = M.insert 'x' (sentCount + 1) $ registers m
applyInstruction (Snd a) other m = (m {pc = pc'}, other ++ [y])
    where pc' = pc m + 1
          y = evaluate m a
          
applyInstruction (Set (Register a) b) other m = (m {registers = reg', pc = pc'}, other)
    where pc' = pc m + 1
          y = evaluate m b
          reg' = M.insert a y $ registers m

applyInstruction (Add (Register a) b) other m = (m {registers = reg', pc = pc'}, other)
    where pc' = pc m + 1
          x = evaluate m (Register a) 
          y = evaluate m b
          reg' = M.insert a (x + y) $ registers m

applyInstruction (Mul (Register a) b) other m = (m {registers = reg', pc = pc'}, other)
    where pc' = pc m + 1
          x = evaluate m (Register a) 
          y = evaluate m b
          reg' = M.insert a (x * y) $ registers m

applyInstruction (Mod (Register a) b) other  m = (m {registers = reg', pc = pc'}, other)
    where pc' = pc m + 1
          x = evaluate m (Register a) 
          y = evaluate m b
          reg' = M.insert a (x `mod` y) $ registers m

applyInstruction (Rcv (Register a)) other m = ( m {registers = reg', messageQueue = mq', pc = pc'}, other)
    where pc' = pc m + 1
          reg' = M.insert a (head $ messageQueue m) $ registers m
          mq' = tail $ messageQueue m
    
applyInstruction (Jgz a b) other m = (m {pc = pc'}, other)
    where x = evaluate m a
          y = evaluate m b
          pc' = if x > 0 then pc m + (fromIntegral y) else pc m + 1

evaluate :: Machine -> Location -> Integer
evaluate _ (Literal i)  = i
evaluate m (Register r) = M.findWithDefault 0 r (registers m)

isReceive :: Instruction -> Bool
isReceive (Rcv _) = True
isReceive _ = False

isSend :: Instruction -> Bool
isSend (Snd _) = True
isSend _ = False