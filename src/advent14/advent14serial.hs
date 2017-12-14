import Data.List.Split (chunksOf)
import Data.Char (ord)
import Text.Printf (printf)
import Data.Bits (xor)
import qualified Data.Map.Strict as M
import qualified Data.Graph as G
-- import Control.Parallel.Strategies (parMap, rpar)
type CellMap = M.Map (Int, Int) Bool

puzzleKey = "xlqgujun"

main :: IO ()
main = do
  print $ part1 puzzleKey
  print $ part2 puzzleKey


part1 :: String -> Int
part1 key = sum rowCounts
    where rowCounts = map countSetBits $ binHashes key


part2 :: String -> Int
part2 key = length $ cellEdges cells
    where cells = presentCells $ binHashes key

binHashes :: String -> [String]
binHashes key = map binHash $ rowSpecs key


binHash :: String -> String
binHash = binify . knotHash

numKey :: (Int, Int) -> Int
numKey (r, c) = 128 * r + c


presentCells :: [String] -> CellMap
presentCells bhs = M.fromList [((r, c), True) | r <- [0..127], c <- [0..127], (bhs!!r)!!c == '1']

adjacentCells :: CellMap -> (Int, Int) -> [(Int, Int)]
adjacentCells cells (r, c) = filter (\k -> M.member k cells) possibles
  where possibles = [(r, c - 1), (r, c + 1), (r - 1, c), (r + 1, c)]


cellEdges :: CellMap -> [G.SCC (Int, Int)]
cellEdges cells = G.stronglyConnComp [(k, numKey k, map numKey $ adjacentCells cells k) | k <- M.keys cells]

rowSpecs :: String -> [String]
rowSpecs key = map (((key ++ "-") ++) . show) ([0..127] :: [Integer])

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

-- hexify :: [Int] -> String
-- hexify = concatMap (printf "%02x")

binify :: [Int] -> String
binify = concatMap (printf "%08b")

densify :: [Int] -> [Int]
densify ns = codes
    where chunks = chunksOf 16 ns
          compress = foldl1 xor
          codes = map compress chunks
