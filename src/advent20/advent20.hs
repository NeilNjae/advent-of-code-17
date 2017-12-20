{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text (Parser)
import qualified Control.Applicative as CA

import qualified Data.Vector as V

import Data.List 

import qualified Data.Set as S


type Vec = V.Vector Integer

data Particle = Particle 
                    { position :: Vec
                    , velocity :: Vec
                    , acceleration :: Vec
                    } deriving (Show, Eq)


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent20.txt"
        let particles = successfulParse text
        print $ part1 particles
        print $ part2 500 particles


part1 :: [Particle] -> Int
part1 particles = head $ withMinX $ simulate particles

part2 :: Integer -> [Particle] -> Int
part2 n particles = length $ simulateC n particles

simulate :: [Particle] -> [Particle]
simulate particles = 
    if all quiescent particles && length withMinXs == 1
    then particles
    else simulate (map step particles)
    where withMinXs = withMinX particles


simulateC :: Integer -> [Particle] -> [Particle]
simulateC 0 particles = particles
simulateC t particles = simulateC (t - 1) (map step particles')
    where particles' = removeColliders particles


step :: Particle -> Particle
step particle = particle {position = p', velocity = v'}
    where pv' = V.zipWith3 updatePV (position particle) (velocity particle) (acceleration particle)
          (p', v') = V.unzip pv'
          updatePV p v a = (p + v + a, v + a)


-- Checks whether a particle could ever get closer to the origin than it is now.
quiescent :: Particle -> Bool
quiescent particle = and qDimensions
    where qDimensions = V.zipWith3 sameSigns (position particle) (velocity particle) (acceleration particle)
          sameSigns p v a = if a == 0 && v == 0
                            then True
                            else if a == 0
                                 then signum p == signum v
                                 else signum p == signum v && signum v == signum a


withMinX particles = minX `elemIndices` absXs
    where absXs = map pAbsX particles
          minX = minimum absXs

pAbsX :: Particle -> Integer
pAbsX particle = V.foldl1' (+) $ V.map abs (position particle)  


removeColliders particles = particles'
    where positions = map position particles
          duplicatePositions = S.fromList $ concat $ filter (\g -> length g > 1) $ group $ sort positions
          particles' = filter (\p -> not (S.member (position p) duplicatePositions)) particles



sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

lexeme  = L.lexeme sc

integer       = lexeme L.integer
signedInteger = L.signed sc integer

symbol = L.symbol sc
separator = symbol ", "
comma = symbol ","

particlesP = particleP `sepBy` space
particleP = particlify <$> (symbol "p=" *> vecP <* separator)
                       <*> (symbol "v=" *> vecP <* separator)
                       <*> (symbol "a=" *> vecP)
    where particlify p v a = Particle {position = p, velocity = v, acceleration = a}


vecP = V.fromList <$> between (symbol "<") (symbol ">") (signedInteger `sepBy` comma)


successfulParse :: Text -> [Particle]
successfulParse input = 
        case parse particlesP "input" input of
                Left  _error -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right instructions  -> instructions           