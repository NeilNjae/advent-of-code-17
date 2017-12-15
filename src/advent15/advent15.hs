import Data.Word
import Data.Bits

generatorAStart = 873
generatorBStart = 583

main :: IO ()
main = do
    print $ part1 
    print $ part2 

part1 = length $ filter (uncurry (==)) $ take 40000000 $ zip streamA streamB


part2 = length $ filter (uncurry (==)) $ take 5000000 $ zip fsA fsB
    where fsA = filteredStream 3 streamA
          fsB = filteredStream 7 streamB


generatorA = generator 2147483647 16807
generatorB = generator 2147483647 48271

streamA = stream generatorA generatorAStart
streamB = stream generatorB generatorBStart

generator :: Word64 -> Word64 -> Word64 -> Word64
generator divisor factor n = fromIntegral $ fromIntegral n * factor `rem` divisor

toWord16 :: Word64 -> Word16
toWord16 = fromIntegral

stream :: (Word64 -> Word64) -> Word64 -> [Word16]
stream gen n0 = map toWord16 $ drop 1 $ iterate gen n0

filteredStream :: Word16 -> [Word16] -> [Word16]
filteredStream f str = filter (\n -> n .&. f == 0) str

