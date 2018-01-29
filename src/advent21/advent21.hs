{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text (Parser)
import qualified Control.Applicative as CA

import qualified Data.Map.Strict as M

import Data.List 


type Grid = M.Map (Int, Int) Bool
type ExplodedGrid = M.Map (Int, Int) Grid

data Rule = Rule Grid Grid deriving (Eq, Show)

rulePre (Rule g _) = g
rulePost (Rule _ g) = g


initialGrid = case parse gridP "" ".#./..#/###" of 
                Left _ -> M.empty 
                Right g -> g


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent21.txt"
        let rules = readRules text
        print $ countLit $ nthApplication rules 5
        print $ countLit $ nthApplication rules 18


-- Read the rules, and expand them to all equivalent left hand sides
readRules :: Text -> [Rule]
readRules = expandRules . successfulParse


expandRules :: [Rule] -> [Rule]
expandRules = concatMap expandRule

expandRule :: Rule -> [Rule]
expandRule rule = [Rule l (rulePost rule) | l <- allArrangements (rulePre rule)]


reflectH :: Grid -> Grid
reflectH g = M.fromList [((r, c) , M.findWithDefault False (rm - r, c) g) | r <- [0..rm], c <- [0..cm] ]
    where (rm, cm) = bounds g

reflectV :: Grid -> Grid
reflectV g = M.fromList [((r, c) , M.findWithDefault False (r, cm - c) g) | r <- [0..rm], c <- [0..cm] ]
    where (rm, cm) = bounds g

-- awkward naming to avoid clashing with Prelude
transposeG :: Grid -> Grid
transposeG g = M.fromList [((c, r) , M.findWithDefault False (r, c) g) | r <- [0..rm], c <- [0..cm] ]
    where (rm, cm) = bounds g


-- Find all the arrangments of a grid, including reflection and rotation.
allArrangements :: Grid -> [Grid]
-- allArrangements grid = map (\f -> f grid) [ id
allArrangements grid = map ($ grid) [ id
                                    , reflectH
                                    , reflectV
                                    , transposeG
                                    , reflectH . transposeG
                                    , reflectV . transposeG
                                    , reflectH . reflectV . transposeG
                                    , reflectV . reflectH
                                    ]



-- Count number of lit pixels
countLit :: Grid -> Int
countLit = M.size . M.filter id

-- apply the rules _n_ times
nthApplication :: [Rule] -> Int -> Grid
nthApplication rules n = (!! n) $ iterate (applyOnce rules) initialGrid

-- Apply one step of the expansion
applyOnce :: [Rule] -> Grid -> Grid
applyOnce rules g = contractExploded $ M.map (apply rules) $ explodeGrid g

-- find the appropriate rule and apply it to a grid
apply :: [Rule] -> Grid -> Grid
apply rules grid = rulePost thisRule
    where ri = head $ findIndices (\r -> rulePre r == grid) rules
          thisRule = rules!!ri


-- create the appropriate subgrids of a grid
explodeGrid :: Grid -> ExplodedGrid
explodeGrid g = if (rm + 1) `rem` 2 == 0 
                then explodeGrid' 2 g
                else explodeGrid' 3 g
    where (rm, _cm) = bounds g

explodeGrid' :: Int -> Grid -> ExplodedGrid
explodeGrid' n g = M.fromList [((bigR, bigC), subGrid n g bigR bigC) | bigR <- [0..bigRm], bigC <- [0..bigCm]]
    where (rm, cm) = bounds g
          bigRm = (rm + 1) `div` n - 1
          bigCm = (cm + 1) `div` n - 1


subGrid :: Int -> Grid -> Int -> Int -> Grid
subGrid n g bigR bigC = M.fromList [ ((r, c), 
                                      M.findWithDefault False (r + rStep, c + cStep) g) 
                                   | r <- [0..(n - 1)], c <- [0..(n - 1)]
                                   ]
    where rStep = bigR * n
          cStep = bigC * n

-- merge a set of subgrids into one
contractExploded :: ExplodedGrid -> Grid
contractExploded gs = foldl1 (>|<) $ map (foldl1 (>-<)) rows
    where rows = explodedRows gs

-- find the rows of an exploded grid
explodedRows :: ExplodedGrid -> [ExplodedGrid]
explodedRows eg = [M.filterWithKey (\(r, _) _ -> r == row) eg | row <- [0..rowMax] ]
    where (rowMax, _) = bounds eg

-- merge two grids horizontally
(>-<) :: Grid -> Grid -> Grid
(>-<) g1 g2 = M.union g1 g2'
    where (_, cm) = bounds g1
          g2' = M.mapKeys (\(r, c) -> (r, c + cm + 1)) g2

-- merge two grids vertically
(>|<) :: Grid -> Grid -> Grid
(>|<) g1 g2 = M.union g1 g2'
    where (rm, _) = bounds g1
          g2' = M.mapKeys (\(r, c) -> (r + rm + 1, c)) g2      




bounds :: M.Map (Int, Int) a -> (Int, Int)
bounds grid = (maximum $ map fst $ M.keys grid, maximum $ map snd $ M.keys grid)


showGrid :: Grid -> String
showGrid g = unlines [[showGChar $ M.findWithDefault False (r, c) g | 
                c <- [0..cm] ] | r <- [0..rm] ]
    where (rm, cm) = bounds g
          showGChar True = '#'
          showGChar False = '.'



-- really persuade Megaparsec not to include newlines in how it consume spaces.
onlySpace = (char ' ') <|> (char '\t')

sc :: Parser ()
sc = L.space (skipSome onlySpace) CA.empty CA.empty

symbol = L.symbol sc
rowSep = symbol "/"
ruleJoin = symbol "=>"

present = id True <$ symbol "#"
absent = id False <$ symbol "."

rulesP = ruleP `sepBy` space
ruleP = Rule <$> gridP <* ruleJoin <*> gridP

gridP = gridify <$> rowP `sepBy` rowSep
    where gridify g = M.fromList $ concat 
                                    [map (\(c, v) -> ((r, c), v)) nr | 
                                             (r, nr) <- zip [0..] 
                                                            [zip [0..] r | r <- g]]


rowP = some (present <|> absent)
 
successfulParse :: Text -> [Rule]
successfulParse input = 
        case parse rulesP "input" input of
                Left  _error -> [] 
                Right instructions  -> instructions