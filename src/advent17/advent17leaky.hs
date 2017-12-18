{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

import Prelude hiding ((++))
import Data.Vector.Unboxed ((!), (++))
import qualified Data.Vector.Unboxed as V
import qualified Data.List as L

type RingBuffer = V.Vector Int

initialStepSize :: Int
initialStepSize = 366

initialRingBuffer :: RingBuffer
initialRingBuffer = V.singleton 0


main :: IO ()
main = do 
        print $ part1 initialStepSize 2017
        print $ part2 initialStepSize 50000000


part1 n k = (!1) $! last $! take k $! L.unfoldr ringBufferUnfolder (1, n, initialRingBuffer)


part2 n k = finalBuffer!targetLoc
    where finalBuffer = last $! take k $! L.unfoldr ringBufferUnfolder (1, n, initialRingBuffer)
          zeroLoc = V.head $ V.elemIndices 0 finalBuffer
          targetLoc = (zeroLoc + 1) `rem` (V.length finalBuffer)


updateRingBuffer nextValue stepSize buffer = (buffer', stepSize)
    where !nextPos = (stepSize + 1) `rem` V.length buffer
          (!start, !end) = V.splitAt nextPos buffer
          !buffer' = V.cons nextValue $ end ++ start

ringBufferUnfolder :: (Int, Int, RingBuffer) -> Maybe (RingBuffer, (Int, Int, RingBuffer))
ringBufferUnfolder (nextValue, stepSize, buffer) = Just (buffer', (nextValue + 1, stepSize', buffer'))
    where (!buffer', !stepSize') = updateRingBuffer nextValue stepSize buffer