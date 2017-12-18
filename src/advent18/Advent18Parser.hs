module Advent18Parser (successfulParse, Location(..), Instruction(..)) where

import Data.Text (Text)
import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text (Parser)
import qualified Control.Applicative as CA

data Location = Literal Integer | Register Char deriving (Show, Eq)
data Instruction =   Snd Location
                   | Set Location Location 
                   | Add Location Location 
                   | Mul Location Location
                   | Mod Location Location
                   | Rcv Location
                   | Jgz Location Location
                   deriving (Show, Eq)


sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

lexeme  = L.lexeme sc

integer       = lexeme L.integer
signedInteger = L.signed sc integer

symb = L.symbol sc
reg = lexeme (some letterChar)

location = (Literal <$> signedInteger) <|> register
register = (Register . head) <$> reg

instructionsP = instructionP `sepBy` space
instructionP = choice [sndP, setP, addP, mulP, modP, rcvP, jgzP]

sndP = Snd <$> (try (symb "snd") *> location)
setP = Set <$> (try (symb "set") *> register) <*> location
addP = Add <$> (try (symb "add") *> register) <*> location
mulP = Mul <$> (try (symb "mul") *> register) <*> location
modP = Mod <$> (try (symb "mod") *> register) <*> location
rcvP = Rcv <$> (try (symb "rcv") *> location)
jgzP = Jgz <$> (try (symb "jgz") *> location) <*> location

successfulParse :: Text -> [Instruction]
successfulParse input = 
        case parse instructionsP "input" input of
                Left  _error -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right instructions  -> instructions