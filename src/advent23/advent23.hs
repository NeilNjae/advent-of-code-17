{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- import Prelude hiding ((++))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text (Parser)
import qualified Control.Applicative as CA

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

import Control.Monad (when)
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer

import qualified Data.Numbers.Primes as P

data Location = Literal Integer | Register Char deriving (Show, Eq)
data Instruction =   Set Location Location 
                   | Sub Location Location 
                   | Mul Location Location
                   | Jnz Location Location
                   deriving (Show, Eq)

data Machine = Machine { registers :: M.Map Char Integer
                       , pc :: Int
                       } 
               deriving (Show, Eq)

type ProgrammedMachine = WriterT [Int] (ReaderT [Instruction] (State Machine)) ()

emptyMachine = Machine {registers = M.empty, pc = 0}



main :: IO ()
main = do 
        text <- TIO.readFile "data/advent23.txt"
        let instrs = successfulParse text
        let ((result, l), machinef) = part1 instrs
        print $ length l
        print $ part2


part1 instructions = 
    runState (
        runReaderT (
            runWriterT executeInstructions
                   ) 
            instructions 
             ) 
             emptyMachine


-- Part 2 following results of analysis by Dario Petrillo
-- https://github.com/dp1/AoC17/blob/master/day23.5.txt
part2 = length $ filter (not . P.isPrime) [start, start + 17 .. end]
    where start = 84 * 100 + 100000
          end = start + 17000


executeInstructions = 
    do  instrs <- ask
        m <- get
        when (pc m >= 0 && pc m < length instrs)
            $
            do when (isMul $ instrs !! pc m) (tell [1])
               executeInstruction
               executeInstructions

executeInstruction :: ProgrammedMachine
executeInstruction =
    do  instrs <- ask
        m <- get
        let instr = instrs!!(pc m)
        put (applyInstruction instr m)


applyInstruction :: Instruction -> Machine -> Machine

applyInstruction (Set (Register a) b) m = m {registers = reg', pc = pc'}
    where pc' = pc m + 1
          y = evaluate m b
          reg' = M.insert a y $ registers m

applyInstruction (Sub (Register a) b) m = m {registers = reg', pc = pc'}
    where pc' = pc m + 1
          x = evaluate m (Register a) 
          y = evaluate m b
          reg' = M.insert a (x - y) $ registers m

applyInstruction (Mul (Register a) b) m = m {registers = reg', pc = pc'}
    where pc' = pc m + 1
          x = evaluate m (Register a) 
          y = evaluate m b
          reg' = M.insert a (x * y) $ registers m

applyInstruction (Jnz a b) m = m {pc = pc'}
    where x = evaluate m a
          y = evaluate m b
          pc' = if x /= 0 then pc m + (fromIntegral y) else pc m + 1


isMul :: Instruction -> Bool
isMul (Mul _ _ ) = True
isMul _ = False

evaluate :: Machine -> Location -> Integer
evaluate _ (Literal i)  = i
evaluate m (Register r) = M.findWithDefault 0 r (registers m)



sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

lexeme  = L.lexeme sc

integer       = lexeme L.integer
signedInteger = L.signed sc integer

symbol = L.symbol sc

-- reg :: Parser String
-- reg = id <$> some letterChar

reg = lexeme (some letterChar)

location = (Literal <$> signedInteger) <|> register
register = (Register . head) <$> reg

instructionsP = instructionP `sepBy` space
instructionP = choice [setP, subP, mulP, jnzP]

setP = Set <$> (try (symbol "set") *> register) <*> location
subP = Sub <$> (try (symbol "sub") *> register) <*> location
mulP = Mul <$> (try (symbol "mul") *> register) <*> location
jnzP = Jnz <$> (try (symbol "jnz") *> location) <*> location

successfulParse :: Text -> [Instruction]
successfulParse input = 
        case parse instructionsP "input" input of
                Left  _error -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right instructions  -> instructions