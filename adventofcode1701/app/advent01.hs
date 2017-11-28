{-# LANGUAGE NegativeLiterals #-}

module Main(main) where

import Text.Parsec hiding (State)
-- import Text.ParserCombinators.Parsec.Number



main :: IO ()
main = do 
        text <- readFile "data/advent01.txt"
        let instructions = successfulParse $ parseIline text
        part1 instructions
        part2 instructions

part1 :: [Int] -> IO ()
part1 instructions = do
        print $ sum instructions

part2 :: [Int] -> IO ()
part2 instructions = do
        print $ length $ takeWhile (> -1) $ scanl (+) 0 instructions



-- instructionFile = instructionLine `endBy` newline 
instructionLine = many (up <|> down)


up   = char '(' *> pure 1
down = char ')' *> pure -1

-- parseIfile :: String -> Either ParseError [[Int]]
-- parseIfile input = parse instructionFile "(unknown)" input

parseIline :: String -> Either ParseError [Int]
parseIline input = parse instructionLine "(unknown)" input

successfulParse :: Either ParseError [a] -> [a]
successfulParse (Left _) = []
successfulParse (Right a) = a
