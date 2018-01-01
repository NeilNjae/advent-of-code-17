import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text (Parser)
import qualified Control.Applicative as CA

import Data.List (nub)

type Position = (Integer, Integer)

(+:) (a, b) (c, d) = (a + c, b + d)



main :: IO ()
main = do 
        text <- TIO.readFile "data/infi.txt"
        let (starts, unchunkedSteps) = successfulParse text
        let steps = chunks (length starts) unchunkedSteps
        let points = visited starts steps
        print $ part1 points
        putStrLn $ part2 points


visited :: [Position] -> [[Position]] -> [[Position]]
visited = scanl (zipWith (+:))

intersections :: [[Position]] -> [[Position]]
intersections = filter ((== 1) . length . nub)

part1 :: [[Position]] -> Int
part1 = length . intersections

part2 :: [[Position]] -> String
part2 points = showPoints bds $ nub $ concat $ intersections points
    where bds = bounds $ nub $ concat points

chunks :: Int -> [b] -> [[b]]
chunks n xs = (take n xs) : if null xs' then [] else chunks n xs'
    where xs' = drop n xs

bounds :: [Position] -> (Integer, Integer, Integer, Integer)
bounds ps = ( minimum $ map fst ps
            , maximum $ map fst ps
            , minimum $ map snd ps
            , maximum $ map snd ps
            )


showPoints :: (Integer, Integer, Integer, Integer) -> [Position] -> String
showPoints (minr, maxr, minc, maxc) ps = unlines [ [ if (r, c) `elem` ps then '*' else ' ' | r <- [minr..maxr] ] | c <- [minc..maxc] ]


sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

lexeme = L.lexeme sc
integer = lexeme L.integer
signedInteger = L.signed sc integer
symbol = L.symbol sc
comma = symbol ","

pointP :: Parser Position
pointP = (,) <$> signedInteger <* comma <*> signedInteger

startPosP = between (symbol "[") (symbol "]") pointP
stepP = between (symbol "(") (symbol ")") pointP

descriptionP = (,) <$> (some startPosP) <*> (some stepP)

successfulParse :: Text -> ([Position], [Position])
successfulParse input = 
        case parse descriptionP "input" input of
                Left  _error -> ([], [])
                Right description -> description
