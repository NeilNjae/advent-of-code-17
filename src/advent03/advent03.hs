import Data.List (tails)
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict ((!))
import Control.Monad.State.Lazy

type Location = (Int, Int)
type Memory = M.HashMap Location Int

target :: Int
target = 347991

main :: IO ()
main = do 
        print $ part1 
        print $ part2


diagonal :: Int -> [Int]
diagonal n = scanl (+) 1 $ scanl (+) n $ repeat 8
dr = diagonal 8
ul = diagonal 4
ur = diagonal 2
dl = diagonal 6


interleave :: [[a]] -> [a]
interleave ([]:_) = []
interleave xss = map head xss ++ interleave (map tail xss)


countedDiags = interleave [(zip [0..] ur), (zip [0..] ul), (zip [0..] dl), (zip [0..] dr)]

part1 = let corners =  head $ dropWhile ((< target) . snd . head . tail) $ tails countedDiags
            (pcd, pcv) = head corners
            (ncd, ncv) = head $ tail corners
            result = if pcd == ncd 
                        then if (target - pcv + 2) < ncv - target
                             then pcd * 2 - (target - pcv)
                             else ncd * 2 - (ncv - target)
                     else if (target - pcv + 1) < ncv - target
                             then pcd * 2 - (target - pcv) + 2
                             else ncd * 2 - (ncv - target)          
    in result


part2 = (!) memoryValues $ head $ dropWhile (\l -> memoryValues!l <= target) locations
    where memoryValues = execState (updateMemory (take 100 $ drop 1 locations)) emptyMemory   

up    (a, b) = (a, b + 1)
down  (a, b) = (a, b - 1)
left  (a, b) = (a - 1, b)
right (a, b) = (a + 1, b)
directions = [right, up, left, down]

locations = scanl (\c f -> f c) (0,0) $ concat $ zipWith replicate steps (cycle directions)
    where
        steps = concat $ zipWith (\a b -> [a,b]) [1..] [1..]

adjacentMap (r, c) = M.filterWithKey adjacent 
    where adjacent k _ = abs (fst k - r) <= 1 && abs (snd k - c) <= 1     

adjacentMapSum here = M.foldr (+) 0 . adjacentMap here


emptyMemory = M.singleton (0, 0) 1  

updateMemoryOnce :: Location -> State Memory Int
updateMemoryOnce here = 
    do m0 <- get
       let total = adjacentMapSum here m0
       put (M.insert here total m0)
       return total

updateMemory :: [Location] -> State Memory Int
updateMemory [] = do return 0
updateMemory (l:ls) = 
    do  updateMemoryOnce l
        updateMemory ls

