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
    where fsA = stream' 4 generatorA generatorAStart -- filteredStream 3 streamA
          fsB = stream' 8 generatorB generatorBStart -- filteredStream 7 streamB


generatorA = generator 2147483647 16807
generatorB = generator 2147483647 48271

streamA = stream generatorA generatorAStart
streamB = stream generatorB generatorBStart

generator :: Int -> Int -> Int -> Int
generator divisor factor n = n * factor `rem` divisor

toWord16 :: Int -> Word16
toWord16 = fromIntegral

stream :: (Int -> Int) -> Int -> [Word16]
stream gen n0 = map toWord16 $ drop 1 $ iterate gen n0

stream' :: Int -> (Int -> Int) -> Int -> [Word16]
stream' f gen n0 = map toWord16 $ filter ((== 0) . (`mod` f)) $ drop 1 $ iterate gen n0


-- filteredStream :: Word16 -> [Word16] -> [Word16]
-- filteredStream f = filter ((== 0) . ( .&. f))

