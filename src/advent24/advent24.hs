{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text (Parser)
import qualified Control.Applicative as CA

import qualified Data.MultiSet as B -- B for bag
import qualified Data.Set as S
import Data.Either

type Part = B.MultiSet Integer
type Parts = B.MultiSet Part
type Candidates = S.Set Part
data Bridge = Bridge { bridgeParts :: Parts, requiring :: Integer } deriving (Eq, Show, Ord)
type Bridges = S.Set Bridge


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent24.txt"
        let parts = successfulParse text
        let bridges = allBridges parts
        print $ part1 bridges
        print $ part2 bridges


part1 = strongestBridge

part2 = bestBridge

strongestBridge :: Bridges -> Integer
strongestBridge bridges = S.findMax $ S.map bridgeStrength bridges

bestBridge :: Bridges -> Integer
bestBridge bridges = strongestBridge longBridges
    where longest = S.findMax $ S.map bridgeLength bridges
          longBridges = S.filter (\b -> bridgeLength b == longest) bridges


emptyBridge :: Bridge
emptyBridge = Bridge { bridgeParts = B.empty, requiring = 0}


allBridges :: Parts -> Bridges
allBridges parts = extendBridges parts (S.singleton emptyBridge) S.empty

extendBridges :: Parts -> Bridges -> Bridges -> Bridges
extendBridges parts bridges completed = 
    if S.null bridges then completed
                      else extendBridges parts bridges' completed'
    where updates = map (extendOneBridge parts) $ S.toList bridges
          newCompleted = lefts updates
          completed' = S.union completed $ S.fromList newCompleted
          bridges' = S.unions $ rights updates

extendOneBridge :: Parts -> Bridge -> Either Bridge Bridges
extendOneBridge parts bridge = 
    if S.null $ candidates parts bridge
    then Left bridge
    else Right (S.map (grow bridge) $ candidates parts bridge)

grow :: Bridge -> Part -> Bridge
grow bridge part = bridge {bridgeParts = bp', requiring = req'}
    where req = requiring bridge
          req' = B.findMin $ B.delete req part -- can get away with `findMin` as I know there are only two elements in a `Part`
          bp' = B.insert part $ bridgeParts bridge

candidates :: Parts -> Bridge -> Candidates
candidates parts bridge = B.toSet $ B.filter canUse parts
    where needed = requiring bridge
          canUse p = hasPort p needed && available parts p bridge

hasPort :: Part -> Integer -> Bool
hasPort part port = port `B.member` part

available :: Parts -> Part -> Bridge -> Bool
available parts part bridge = B.occur part parts > B.occur part (bridgeParts bridge)


bridgeStrength :: Bridge -> Integer
bridgeStrength bridge = B.fold (+) 0 $ B.map partStrength $ bridgeParts bridge
    where partStrength = sum . B.elems 

bridgeLength :: Bridge -> Int
bridgeLength bridge = B.size $ bridgeParts bridge


-- really persuade Megaparsec not to include newlines in how it consume spaces.
onlySpace = (char ' ') <|> (char '\t')

sc :: Parser ()
sc = L.space (skipSome onlySpace) CA.empty CA.empty

lexeme = L.lexeme sc
integer = lexeme L.integer
symbol = L.symbol sc
slash = symbol "/"

partsP = B.fromList <$> partP `sepBy` newline
partP = B.fromList <$> integer `sepBy` slash

successfulParse :: Text -> Parts
successfulParse input = 
        case parse partsP "input" input of
                Left  _error -> B.empty
                Right partsList -> partsList