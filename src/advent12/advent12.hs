{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text (Parser)

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

import qualified Data.Set as S
import Control.Applicative (empty)


type ProgSet = S.Set Integer
type Pipes = M.Map Integer ProgSet


main :: IO ()
main = do 
        input <- TIO.readFile "data/advent12.txt"
        let pipes = successfulParse input
        print $ part1 pipes
        print $ part2 pipes


part1 pipes = S.size $ reachable pipes (S.empty) (pipes!0)

part2 pipes = n
    where (_, n, _) = foldl addGroup (S.empty, 0, pipes) $ M.keys pipes 


addGroup :: (ProgSet, Integer, Pipes) -> Integer -> (ProgSet, Integer, Pipes)
addGroup (done, n, pipes) p
    | p `S.member` done = (done, n, pipes)
    | otherwise = (S.union done reached, n + 1, pipes)
        where reached = reachable pipes (S.empty) (pipes!p)


reachable :: Pipes -> ProgSet -> ProgSet -> ProgSet
reachable pipes reached frontier
    | S.null frontier = reached
    | otherwise = reachable pipes reached' frontier'
        where frontier' = S.difference (unions' $ S.map (\p -> pipes!p) frontier) reached
              reached' = reached `S.union` frontier'
              unions' = S.foldl S.union S.empty



sc :: Parser ()
sc = L.space (skipSome spaceChar) empty empty -- lineCmnt blockCmnt
  -- where
  --   lineCmnt  = L.skipLineComment "//"
  --   blockCmnt = L.skipBlockComment "/*" "*/"

lexeme  = L.lexeme sc
integer = lexeme L.integer
symb = L.symbol sc


pipesP = many pipe

pipe = assocify <$> integer <*> (symb "<->" *> (integer `sepBy1` (symb ",")))
    where assocify a b = (a, S.fromList b)

successfulParse :: Text -> Pipes
successfulParse input = 
        case parse pipesP "input" input of
                Left  err   -> M.empty -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right betterInput -> M.fromList betterInput


-- sample = T.pack "0 <-> 2\n\
-- \1 <-> 1\n\
-- \2 <-> 0, 3, 4\n\
-- \3 <-> 2, 4\n\
-- \4 <-> 2, 3, 6\n\
-- \5 <-> 6\n\
-- \6 <-> 4, 5"