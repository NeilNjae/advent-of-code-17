import Data.List (sort, nub)

main :: IO ()
main = do 
        text <- readFile "data/advent04.txt"
        let passphrases = map words $ lines text
        print $ part1 passphrases
        print $ part2 passphrases

part1 :: [[String]] -> Int
part1 = length . filter (not . containsDuplicates) 

part2 :: [[String]] -> Int
part2 = length . filter (not . containsAnagrams) 

containsDuplicates :: [String] -> Bool
containsDuplicates passphrase = (length passphrase) /= (length $ nub passphrase)

containsAnagrams :: [String] -> Bool
containsAnagrams = containsDuplicates . (map sort)
