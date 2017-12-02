module Main(main) where

import Text.Parsec 
import Text.ParserCombinators.Parsec.Number


main :: IO ()
main = do 
        text <- readFile "data/advent02.txt"
        let sheet = successfulParse $ parseFile text
        print $ part1 sheet
        print $ part2 sheet


part1 :: [[Int]] -> Int
part1 = sum . map p1cSum

part2 :: [[Int]] -> Int
part2 = sum . map p2cSum


p1cSum :: [Int] -> Int
p1cSum row = (maximum row) - (minimum row)

p2cSum :: [Int] -> Int
p2cSum digits = sum [a `div` b | a <- digits, b <- digits, a /= b, a `mod` b == 0]



sFile = sLine `sepEndBy` newline 
sLine = int `sepBy` onlySpaces

onlySpaces = many (oneOf " \t")

parseFile :: String -> Either ParseError [[Int]]
parseFile input = parse sFile "(unknown)" input

parseLine :: String -> Either ParseError [Int]
parseLine input = parse sLine "(unknown)" input

successfulParse :: Either ParseError [a] -> [a]
successfulParse (Left _) = []
successfulParse (Right a) = a

