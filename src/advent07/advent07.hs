import Text.Parsec 
import Text.ParserCombinators.Parsec.Number
import Data.List (sort, group)
import qualified Data.Set as S

data Program = Program String Int [String]
                deriving (Show, Eq)

name (Program n _ _) = n 
weight (Program _ w _) = w
supports (Program _ _ s) = s

data Tree = Tree Program [Tree] Int deriving (Show, Eq)
root (Tree p _ _) = p
branches (Tree _ b _) = b
tWeight (Tree _ _ w) = w



main :: IO ()
main = do 
        text <- readFile "data/advent07.txt"
        let progs = successfulParse $ parseFile text
        print $ part1 progs
        print $ part2 progs


part1 :: [Program] -> String
part1 progs = head $ S.elems $ S.difference pr su
    where su = supported progs
          pr = allPrograms progs


part2 programs = (weight $ root problem) - wrongWeight + rightWeight
    where tree = mkTree (findByName (part1 programs) programs) programs
          problem = problemTree tree
          pt = problemParent problem tree
          wrongWeight = problemWeight pt
          rightWeight = notProblemWeight pt


allPrograms :: [Program] -> S.Set String
allPrograms = S.fromList . map name

supported :: [Program] -> S.Set String
supported = S.unions . map (S.fromList . supports)


-- leaves :: [Program] -> [Program]
-- leaves = filter (null . supports)


mkTree :: Program -> [Program] -> Tree
mkTree program programs = Tree program subTrees (weight program + w)
    where subPrograms = map (\n -> findByName n programs) $ supports program
          subTrees = map (\r -> mkTree r programs) subPrograms
          w = sum $ map tWeight subTrees

findByName :: String -> [Program] -> Program
findByName n programs = head $ filter (\p -> n == (name p)) programs 



balanced :: Tree -> Bool
balanced t = (S.size $ S.fromList $ map tWeight $ branches t) <= 1


problemTree :: Tree -> Tree
problemTree t 
    | balanced t = t
    | otherwise = problemTree problemSubtree
        where subtreeWeights = map tWeight $ branches t
              weightGroups = group $ sort subtreeWeights
              pWeight = head $ head $ filter (\g -> length g == 1) weightGroups
              problemSubtree = head $ filter (\s -> tWeight s == pWeight) (branches t)


problemParent :: Tree -> Tree -> Tree
problemParent problem tree = head $ problemParent' problem tree

problemParent' :: Tree -> Tree -> [Tree]
problemParent' problem tree
    | problem `elem` (branches tree) = [tree]
    | null $ branches tree = []
    | otherwise = concatMap (problemParent' problem) $ branches tree


problemWeight :: Tree -> Int
problemWeight tree = head $ head $ filter (\g -> 1 == length g) $ group $ sort $ map tWeight $ branches tree

notProblemWeight :: Tree -> Int
notProblemWeight tree = head $ head $ filter (\g -> 1 /= length g) $ group $ sort $ map tWeight $ branches tree



onlySpaces = many (oneOf " \t")
parens = between (string "(") (string ")")
symP = many lower
commaSep sym = sym `sepBy` (onlySpaces *> string "," *> onlySpaces)   

mFile = mLine `sepBy` newline 
mLine = Program <$> symP <*> (onlySpaces *> (parens int)) <*> supportsP
supportsP = (onlySpaces *> (string "->") *> onlySpaces *> (commaSep symP)) <|> (pure [])

parseFile :: String -> Either ParseError [Program]
parseFile input = parse mFile "(unknown)" input

-- parseLine :: String -> Either ParseError Program
-- parseLine input = parse mLine "(unknown)" input

successfulParse :: Either ParseError [a] -> [a]
successfulParse (Left _) = []
successfulParse (Right a) = a



-- sampleT = "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"
-- sample = successfulParse $ parseFile sampleT

-- sampleLeaves = leaves sample
-- sampleBranch = sample \\ sampleLeaves
