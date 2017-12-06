import qualified Data.Vector as V
import Data.Vector ((//), (!))
import Data.List (unfoldr)
import qualified Data.Map.Strict as M

type Memory = V.Vector Int
type Redist = (Int, Int, Memory)
type History = M.Map Memory Int

main :: IO ()
main = do 
        text <- readFile "data/advent06.txt"
        let memory = map read $ words text
        print $ part1 memory
        print $ part2 memory

part1 :: [Int] -> Int
part1 = (findRepeat M.empty) . (zip [0..]) . redistSeq . V.fromList

part2 :: [Int] -> Int
part2 = (findRepeatB M.empty) . (zip [0..]) . redistSeq . V.fromList

findRepeat :: History -> [(Int, Memory)] -> Int
findRepeat h ((n, x) : nxs) = if x `M.member` h 
                              then n + 1
                              else findRepeat (M.insert x n h) nxs

findRepeatB :: History -> [(Int, Memory)] -> Int
findRepeatB h ((n, x) : nxs) = if x `M.member` h 
                              then n - n0
                              else findRepeatB (M.insert x n h) nxs
    where n0 = (M.!) h x


redistSeq :: Memory -> [Memory]
redistSeq = unfoldr redistU
    where redistU vec = Just (redist vec, redist vec)

redist :: Memory -> Memory
redist = redistR . redistStart



redistStart :: Memory -> Redist
redistStart vec0 = (current, toDistribute, startVec)
    where origin = V.maxIndex vec0
          toDistribute = vec0!origin
          current = (origin + 1) `mod` (length vec0)
          startVec = vec0 // [(origin, 0)] 

redistR :: Redist -> Memory
redistR (_, 0, vec) = vec
redistR (i, n, vec) = redistR (i', n', vec')
    where n' = n - 1
          i' = (i + 1) `mod` (length vec)
          vec' = vec // [(i, vec!i + 1)]