{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text (Parser)
import qualified Control.Applicative as CA

import qualified Data.Map as M
import Data.Map ((!))

import Control.Monad (unless)
import Control.Monad.State.Lazy
import Control.Monad.Reader

type TuringState = String

type Tape = M.Map Integer Bool

data StateTransition = StateTransition { writeValue :: Bool
                                       , newState :: TuringState
                                       , tapeMovement :: Integer
                                       } deriving (Show, Eq)

type RuleTrigger = (TuringState, Bool)

type Rules = M.Map RuleTrigger StateTransition

data Machine = Machine { tState :: TuringState
                       , tape :: Tape
                       , tapeLocation :: Integer
                       , stepsRemaining :: Integer
                       } 
               deriving (Show, Eq)

emptyMachine = Machine {tState = "unknown", tape = M.empty, tapeLocation = 0, stepsRemaining = 0}

type ProgrammedMachine = ReaderT Rules (State Machine) ()


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent25.txt"
        let (machine0, rules) = successfulParse text
        let machinef = part1 rules machine0
        print $ M.size $ M.filter id $ tape machinef


part1 :: Rules -> Machine -> Machine
part1 rules machine0 = 
    execState (
        runReaderT executeSteps
                   rules 
             ) 
             machine0

executeSteps :: ProgrammedMachine
executeSteps = 
    do m <- get
       unless (stepsRemaining m == 0) $
           do  executeStep
               executeSteps


executeStep :: ProgrammedMachine
executeStep = 
    do rules <- ask
       m <- get
       let tapeHere = M.findWithDefault False (tapeLocation m) (tape m)
       let transition = rules!(tState m, tapeHere)
       let tape' = M.insert (tapeLocation m) (writeValue transition) (tape m)
       let loc' = (tapeLocation m) + (tapeMovement transition)
       let tState' = newState transition
       let steps' = stepsRemaining m - 1
       let m' = m {tState = tState', tape = tape', tapeLocation = loc', stepsRemaining = steps'}
       put m'



sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

lexeme = L.lexeme sc
integer = lexeme L.integer
symbol = L.symbol sc
fullstop = symbol "."

commandP = between (symbol "-") fullstop

writeValueP = (symbol "1" *> pure True) <|> (symbol "0" *> pure False)
writeP = commandP ((symbol "Write the value") *> writeValueP)

directionP = (symbol "left" *> pure -1) <|> (symbol "right" *> pure 1)
tapeMovementP = commandP ((symbol "Move one slot to the") *> directionP)

newStateP = commandP ((symbol "Continue with state") *> (some letterChar))

stateTransitionP = stify <$> writeP <*> tapeMovementP <*> newStateP
    where stify w t s = StateTransition {writeValue = w, newState = s, tapeMovement = t}
    
currentValueP = (symbol "If the current value is") *> writeValueP <* (symbol ":")
    
stateWhenP = (,) <$> currentValueP <*> stateTransitionP
    
stateDefP = (symbol "In state") *> (some letterChar) <* (symbol ":")
    
stateRulesP = rulify <$> stateDefP <*> (stateWhenP `sepBy` space)
    where rulify s ts = M.fromList $ map (\(v, t) -> ((s, v), t)) ts
   
manyStateRulesP = M.unions <$> (stateRulesP `sepBy` space)

startStateP = (symbol "Begin in state") *> (some letterChar) <* fullstop
stepsP =  (symbol "Perform a diagnostic checksum after") *> integer <* (symbol "steps") <* fullstop

machineDescriptionP = machineify <$> startStateP <*> stepsP <*> manyStateRulesP
    where machineify initial limit rules = 
            ( emptyMachine { tState = initial, stepsRemaining = limit }
            , rules
            )

successfulParse :: Text -> (Machine, Rules)
successfulParse input = 
        case parse machineDescriptionP "input" input of
                Left  _error -> (emptyMachine, M.empty)
                Right machineRules -> machineRules