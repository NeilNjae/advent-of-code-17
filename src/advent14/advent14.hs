import Data.List.Split (chunksOf)
import Data.Char (ord)
import Text.Printf (printf)
import Data.Bits (xor)

puzzleKey = "xlqgujun"


part1 :: String -> Int
part1 key = sum rowCounts
    where hashes = map knotHash $ rowSpecs key
          rowCounts = map (countSetBits . binify) hashes

rowSpecs :: String -> [String]
rowSpecs key = map (((key ++ "-") ++) . show) [0..127]

countSetBits :: String -> Int
countSetBits = length . filter (== '1')


knotHash :: String -> [Int]
knotHash input = densify tied
    where (tied, _, _) = foldl step ([0..255], 0, 0) hashTerms
          hashTerms = mkHashTerms input

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


mkHashTerms :: String -> [Int]
mkHashTerms text = take (length chunk * 64) $ cycle chunk
    where chunk = map ord text ++ [17, 31, 73, 47, 23]

hexify :: [Int] -> String
hexify = concatMap (printf "%02x")

binify :: [Int] -> String
binify = concatMap (printf "%08b")

densify :: [Int] -> [Int]
densify ns = codes
    where chunks = chunksOf 16 ns
          compress = foldl1 xor
          codes = map compress chunks