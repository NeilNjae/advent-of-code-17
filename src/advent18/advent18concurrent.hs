import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

import Control.Monad (when, unless)
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer

import Control.Concurrent.Classy.Chan

import Advent18Parser


type Messages = State (Chan Integer)

data Machine = Machine { registers :: M.Map Char Integer
                       , pc :: Int
                       , receivedMessages :: Messages
                       , sentMessages :: Message
                       } 
               deriving (Show, Eq)

data MachinePair = MachinePair { machine0 :: Machine 
                               , machine1 :: Machine 
                               } deriving (Show, Eq)

type ProgrammedMachine = WriterT [String] (ReaderT [Instruction] (State MachinePair)) ()


emptyMachine rq sq = Machine { registers = M.empty, pc = 0
                             , receivedMessages = rq
                             , sentMessages = sq
                             }


emptyMachinePair c01 c10 = MachinePair { machine0 = emptyMachine c10 c01 { registers = M.singleton 'p' 0 }
                                       , machine1 = emptyMachine c01 c10 { registers = M.singleton 'p' 1 }
                                       }


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent18.txt"
        let instrs = successfulParse text
        let ((result, l), statef) = part2 instrs
        print $ length l

part2 :: [Instruction] -> (((), [String]), Machine)
part2 instructions = 
    runState (
        runReaderT (
            runWriterT executeInstructions
                   ) 
            instructions 
             ) 
             emptyMachine

setupMachines = 
    do  p0p1 <- newChan 
        p1p0 <- newChan
        let mp = empytMachinePair p0p1 p1p0
        put mp



executeInstructions :: ProgrammedMachinePair
executeInstructions = 
    do  instrs <- ask
        m <- get
        when (pc m >= 0 && pc m < length instrs)
                $
                do executeInstruction
                   executeInstructions

executeInstruction :: Bool -> ProgrammedMachinePair
executeInstruction =
    do  instrs <- ask
        m <- get
        let mq = messageQueue m
        let instr = instrs!!(pc m)
        let (ma', mq') = applyInstruction instr mq ma
        let mb' = mb {messageQueue = mq'}
        let mp' = if m0Active then mp {machine0 = ma', machine1 = mb'}
                              else mp {machine0 = mb', machine1 = ma'}
        put mp'

applyInstruction :: Instruction -> [Integer] -> Machine -> (Machine, [Integer])

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