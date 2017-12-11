import Data.List.Split (splitOn)

main :: IO ()
main = do 
        text <- readFile "data/advent11.txt"
        print $ part1 text
        print $ part2 text

part1 :: String -> Int
part1 = distance . hexPath . splitOn ","

part2 :: String -> Int
part2 = maximum . map distance . hexPathB . splitOn ","

hexStep :: (Int, Int) -> String -> (Int, Int)
hexStep (n, ne) s = case s of 
                        "n"  -> (n + 1, ne)
                        "ne" -> (n,     ne + 1)
                        "nw" -> (n + 1, ne - 1)
                        "s"  -> (n - 1, ne)
                        "se" -> (n - 1, ne + 1)
                        "sw" -> (n,     ne - 1)
                        _    -> (n,     ne)

hexPath :: [String] -> (Int, Int)
hexPath  = foldl hexStep (0, 0)

hexPathB :: [String] -> [(Int, Int)]
hexPathB = scanl hexStep (0, 0)

distance :: (Int, Int) -> Int
distance (n, ne) = if n * ne > 0 
                   then (abs n) + (abs ne)
                   else smallest + remainder
                   where smallest = min (abs n) (abs ne)
                         remainder = max ((abs n) - smallest) ((abs ne) - smallest)
