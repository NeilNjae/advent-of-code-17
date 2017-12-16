{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Prelude hiding ((++))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text (Parser)
import qualified Control.Applicative as CA

import Control.Monad.State.Lazy
import Control.Monad.Reader

import Data.Vector.Unboxed ((!), (++), (//))
import qualified Data.Vector.Unboxed as V

import qualified Data.IntMap as M


data Step =   Spin Int
            | Exchange Int Int
            | Partner Char Char
            deriving (Show, Eq)

type Dancers = V.Vector Char

type DanceHistory = M.IntMap Dancers

type HistoryRecorder = ReaderT [Step] (State DanceHistory) DanceHistory


startingDancers :: Dancers
startingDancers = V.fromList ['a'..'p'] 

emptyHistory :: DanceHistory
emptyHistory = M.singleton 0 startingDancers


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent16.txt"
        let instrs = successfulParse text
        print $ part1 instrs
        print $ part2 instrs


part1 :: [Step] -> Dancers
part1 instrs = evalState (runDance instrs) startingDancers

part2 instrs = (M.!) history (1000000000 `rem` M.size history)
    where history = evalState (runReaderT (recordDance startingDancers) instrs) emptyHistory


runDance :: [Step] -> State Dancers Dancers
runDance [] = do dancers <- get
                 return dancers
runDance (step:steps) = 
    do dancers <- get
       let dancers' = case step of
                        Spin n -> spin n dancers
                        Exchange a b -> exchange a b dancers
                        Partner a b -> partner a b dancers
       put dancers'
       runDance steps


recordDance :: Dancers -> HistoryRecorder
recordDance dancers = 
    do
        history <- get
        instrs <- ask
        let dancers' = evalState (runDance instrs) dancers
        if dancers' == startingDancers && (not (history == emptyHistory))
        then return history
        else do 
--                 instrs <- ask
--                 let dancers' = evalState (runDance instrs) dancers
                let history' = M.insert (M.size history) dancers' history
                put history'
                recordDance dancers'

spin :: Int -> Dancers -> Dancers
spin n dancers = back ++ front
    where (front, back) = V.splitAt n' dancers
          n' = V.length dancers - n

exchange :: Int -> Int -> Dancers -> Dancers
exchange a b dancers = dancers // [(a, dancers!b), (b, dancers!a)]

partner :: Char -> Char -> Dancers -> Dancers
partner a b dancers = exchange a' b' dancers
    where a' = V.head $ V.elemIndices a dancers
          b' = V.head $ V.elemIndices b dancers


sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

-- lexeme  = L.lexeme sc

int :: Parser Int
int = read <$> some digitChar

symb = L.symbol sc
comma = char ','
dancer = oneOf ['a'..'p']

stepsP = stepP `sepBy` comma
stepP = (try spinP) <|> (try exchangeP) <|> partnerP

spinP = Spin <$> (symb "s" *> int)
exchangeP = Exchange <$> (symb "x" *> int) <*> (symb "/" *> int)
partnerP = Partner <$> (symb "p" *> dancer) <*> (symb "/" *> dancer)

successfulParse :: Text -> [Step]
successfulParse input = 
        case parse stepsP "input" input of
                Left  _error -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right steps  -> steps