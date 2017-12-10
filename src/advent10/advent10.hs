import Data.List.Split (splitOn, chunksOf)
import Data.Char (ord)
import Data.Bits (xor)
import Text.Printf (printf)

main :: IO ()
main = do 
        text <- readFile "data/advent10.txt"
        let ls = map read $ splitOn "," text
        print $ part1 ls
        putStrLn $ part2 text


part1 :: [Int] -> Int
part1 lengths = (tied!!0) * (tied!!1)
    where (tied, _, _) = foldl step ([0..255], 0, 0) lengths


part2 :: String -> String
part2 text = densify tied
    where lengths = p2lengths text
          (tied, _, _) = foldl step ([0..255], 0, 0) lengths

step :: ([Int], Int, Int) -> Int -> ([Int], Int, Int)
step (original, start, skip) len = (replaced, start', skip + 1)
    where replaced = tie original start len
          start' = (start + len + skip) `mod` (length original)

tie :: [a] -> Int -> Int -> [a]
tie original start len = replace original replacement start
    where replacement = reverse $ extract original start len

extract :: [a] -> Int -> Int -> [a]
extract items from len = take len $ drop from $ items ++ items

replace :: [a] -> [a] -> Int -> [a]
replace original replacement from = take (length original) (start ++ replacement ++ remainder)
    where excess = drop (length original - from) replacement
          stub = drop (length excess) original
          start = take from (excess ++ stub)
          remainder = drop (length $ start ++ replacement) original 


p2lengths :: String -> [Int]
p2lengths text = take (length chunk * 64) $ cycle chunk
    where chunk = map ord text ++ [17, 31, 73, 47, 23]

densify :: [Int] -> String
densify ns = concatMap (printf "%02x") codes
    where chunks = chunksOf 16 ns
          compress = foldl1 xor
          codes = map compress chunks
