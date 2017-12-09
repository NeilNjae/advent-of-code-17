import Text.Parsec 
import Text.ParserCombinators.Parsec.Number
import qualified Data.Map.Strict as M

data Instruction = Instruction 
                    { register :: String
                    , direction :: String
                    , change :: Int
                    , conditionRegister :: String
                    , operation :: String
                    , comparator :: Int
                    } deriving (Show, Eq)

type Memory = M.Map String Int      


main :: IO ()
main = do 
        text <- readFile "data/advent08.txt"
        let instrs = successfulParse $ parseFile text
        print $ part1 instrs
        print $ part2 instrs


part1 :: [Instruction] -> Int
part1 = largestValue . processInstructions

part2 :: [Instruction] -> Int
part2 = fst . processInstructionsH

processInstructions :: [Instruction] -> Memory
processInstructions = foldl processInstruction M.empty 

processInstruction :: Memory -> Instruction -> Memory
processInstruction memory instruction = memory'
    where v = M.findWithDefault 0 (register instruction) memory
          cv = M.findWithDefault 0 (conditionRegister instruction) memory
          condition = conditionEval cv (operation instruction) (comparator instruction)
          delta = effectiveChange (direction instruction) (change instruction)
          memory' = if condition
                    then M.insert (register instruction) (v + delta) memory
                    else memory

processInstructionsH :: [Instruction] -> (Int, Memory)
processInstructionsH = foldl processInstructionH (0, M.empty)

processInstructionH :: (Int, Memory) -> Instruction -> (Int, Memory)
processInstructionH (highest, memory) instruction = (highest', memory')
    where memory' = processInstruction memory instruction
          h = largestValue memory'
          highest' = if h > highest then h else highest

conditionEval :: Int -> String -> Int -> Bool
conditionEval reg op val
    | op == "==" = reg == val
    | op == "<"  = reg < val
    | op == ">"  = reg > val
    | op == "<=" = reg <= val
    | op == ">=" = reg >= val
    | op == "!=" = reg /= val

effectiveChange :: String -> Int -> Int
effectiveChange d val 
    | d == "inc" = val
    | otherwise = -val
-- effectiveChange "dec" val = -val


largestValue :: Memory -> Int
largestValue m 
    | M.null m = 0
    | otherwise = maximum $ M.elems m





onlySpaces = many (oneOf " \t")
symP = (many lower) <* onlySpaces
operationP = (many1 (oneOf "!<>=")) <* onlySpaces

iFile = iLine `sepBy` newline 
iLine = instructify <$> symP 
                    <*> symP 
                    <*> int 
                    <*> ( onlySpaces *> string "if" *> onlySpaces *> symP )
                    <*> operationP 
                    <*> int
    where instructify r d c cr o p = Instruction { register = r
                                                 , direction = d
                                                 , change = c
                                                 , conditionRegister = cr
                                                 , operation = o
                                                 , comparator = p
                                                 }

parseFile :: String -> Either ParseError [Instruction]
parseFile input = parse iFile "(unknown)" input

parseLine :: String -> Either ParseError Instruction
parseLine input = parse iLine "(unknown)" input

successfulParse :: Either ParseError [a] -> [a]
successfulParse (Left _) = []
successfulParse (Right a) = a          