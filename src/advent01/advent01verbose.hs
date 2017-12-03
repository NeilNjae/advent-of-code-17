{-# LANGUAGE NegativeLiterals #-}

module Main(main) where

import Data.List (tails)

main :: IO ()
main = do 
        digits <- readFile "data/advent01.txt"
        print $ part1 digits
        print $ part2 digits

part1 :: String -> Integer  
part1 = sum_valid_pairs . part1_extract

part2 :: String -> Integer  
part2 = sum_valid_pairs . part2_extract

part1_extract :: String -> [String]  
part1_extract digits =  map (take 2) $ tails (digits ++ [head digits])

part2_extract :: String -> [String]
part2_extract digits = map (\ds -> (take 1 ds) ++ (take 1 $ drop offset ds)) 
        $ take (length digits) 
        $ tails (digits ++ digits)
    where offset = length digits `div` 2

sum_valid_pairs :: [String] -> Integer
sum_valid_pairs possibles = sum $ map (read . take 1) 
                   $ filter (\(x:y:_) -> x == y) 
                   $ filter (\p -> length p == 2) possibles
                       