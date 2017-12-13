{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text (Parser)

import qualified Control.Applicative as CA

type ScannerDef = (Integer, Integer)
type Scanner = Integer -> Integer


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent13.txt"
        let scannerDefs = successfulParse text
        print $ part1 scannerDefs
        print $ part2 scannerDefs


part1 :: [ScannerDef] -> Integer
part1 = sum . map (uncurry (*)) . filter (\(d, r) -> scanner d r 0 == 0)


part2 :: [ScannerDef] -> Integer
part2 scannerDefs = head $ filter (canPass scanners) [0..]
    where scanners = scanify scannerDefs


scanify :: [ScannerDef] -> [Scanner]
scanify = map (uncurry scanner)

canPass :: [Scanner] -> Integer -> Bool
canPass scannersF t = all (\s -> s t /= 0) scannersF


scanner :: Integer -> Integer -> Integer -> Integer
scanner depth range t = 
    let t' = (t + depth) `mod` ((range - 1) * 2)
    in if t' < range
       then t' 
       else range - t' - 1 -- t' + (t' - range + 1) * -2


sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.integer
symb = L.symbol sc

scannersP = many scannerP

scannerP = (,) <$> integer <*> (symb ":" *> integer)

successfulParse :: Text -> [ScannerDef]
successfulParse input = 
        case parse scannersP "input" input of
                Left  err   -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right scanners -> scanners     