data ParseState = ParseState
                    { total :: Int
                    , depth :: Int
                    , garbageCount :: Int
                    , readingGarbage :: Bool
                    , ignoreCharacter :: Bool
                    } deriving (Show, Eq)

main :: IO ()
main = do 
        text <- readFile "data/advent09.txt"
        print $ part1 text
        print $ part2 text


part1 :: String -> Int
part1 = total . process

part2 :: String -> Int
part2 = garbageCount . process


process :: String -> ParseState
process = foldl parse ps0
    where ps0 = ParseState {total = 0, depth = 0, garbageCount = 0,
                    readingGarbage = False, ignoreCharacter = False}

parse :: ParseState -> Char -> ParseState
parse ps c 
    | ignoreCharacter ps = ps {ignoreCharacter = False}
    | c == '!' = ps {ignoreCharacter = True}
    | readingGarbage ps = if c == '>'
                          then ps {readingGarbage = False}
                          else ps {garbageCount = garbageCount ps + 1}
    | otherwise = 
        case c of 
            '<' -> ps {readingGarbage = True}
            '{' -> openGroup ps
            '}' -> closeGroup ps
            _   -> ps
                                  
openGroup :: ParseState -> ParseState
openGroup ps = ps {depth = depth ps + 1}

closeGroup :: ParseState -> ParseState
closeGroup ps = ps {total = total ps + depth ps, depth = depth ps - 1}                                 
