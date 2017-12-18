{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

-- import Prelude hiding ((++))
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V
import Data.List (foldl')

type RingBuffer = V.Vector Int

initialStepSize :: Int
initialStepSize = 366

initialRingBuffer :: RingBuffer
initialRingBuffer = V.singleton 0


main :: IO ()
main = do 
        print $ part1 2017
        -- print $ part2 50000000
        print $ ith 2017 366
        print $ oneth 50000000 366



part1 :: Int -> Int
part1 k = (!1) $ (iterate updateRingBuffer initialRingBuffer)!!k

part2 :: Int -> Int
part2 k = finalBuffer!targetLoc
    where finalBuffer = (iterate updateRingBuffer initialRingBuffer)!!k
          zeroLoc = V.head $ V.elemIndices 0 finalBuffer
          targetLoc = (zeroLoc + 1) `rem` (V.length finalBuffer)

updateRingBuffer :: RingBuffer -> RingBuffer
updateRingBuffer buffer = buffer'
    where nextPos = (initialStepSize + 1) `rem` V.length buffer
          (start, end) = V.splitAt nextPos buffer
          nextValue = V.length buffer
          buffer' = V.cons nextValue $ (V.++) end start



(%) = mod

ith :: Int -> Int -> Int
ith i steps =
    let (position, list) = foldl' (\(currPos, currList) n -> 
            let newPos  = (currPos + steps % n + 1) % n
            in  (newPos, take newPos currList ++ [n] ++ drop newPos currList))
            (0, [0]) [1..i]
    in  list !! (position + 1)

oneth :: Int -> Int -> Int
oneth i steps =
    snd $ foldl' (\(currPos, currOneth) n -> 
        let !newPos = (currPos + steps % n + 1) % n
        in  (newPos, if newPos == 0 then n else currOneth)) 
        (0, 1) [1..i]

-- main :: IO ()
-- main = do
--     print $ ith   2017     312
--     print $ oneth 50000000 312