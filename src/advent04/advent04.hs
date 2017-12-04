import Data.List (sort)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

main :: IO ()
main = do 
        text <- readFile "data/advent04.txt"
        let passphrases = map parseLine $ lines text
        print $ part1 passphrases
        print $ part2 passphrases

part1 :: [[String]] -> Int
part1 = length . filter (not . containsDuplicates) 

part2 :: [[String]] -> Int
part2 = length . filter (not . containsAnagrams) 

parseLine :: String -> [String]
parseLine = filter (not . null) . splitOn " "    

frequency :: (Ord a) => [a] -> M.Map a Int 
frequency xs = M.fromListWith (+) [(x, 1) | x <- xs]

containsDuplicates :: [String] -> Bool
containsDuplicates = (not . M.null) . M.filter (> 1) . frequency

containsAnagrams :: [String] -> Bool
containsAnagrams = containsDuplicates . (map sort)
