-- This file will give the mechanics behind the Bean game. The ultimate objective
-- of this project is to develop an AI for the Bean game in order to determine
-- optimal strategies.

import Data.List
import Data.Maybe
import Control.Monad (replicateM)
import System.Random

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

(>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>) f g = g . f

data Card
  = Cocoa
  | Garden
  | Red
  | BlackEyed
  | Soy
  | Green
  | StinkyG
  | Chili
  | Blue
  | Wax
  | Coffee
  deriving (Show, Read, Eq)

beanCounts :: [(Card, Int)]
beanCounts = [ (Cocoa, 4)
             , (Garden, 6)
             , (Red, 8)
             , (BlackEyed, 10)
             , (Soy, 12)
             , (Green, 14)
             , (StinkyG, 16)
             , (Chili, 18)
             , (Blue, 20)
             , (Wax, 22)
             , (Coffee, 24)
             ]

baseDeck = concat repeatedBeans
  where repeatedBeans = beanCounts |> map (\(b, n) -> replicate n b)

type Deck = [Card]
type Hand = [(Card, Int)]
type Score = Int
data Field =
    Field2 (Maybe (Card, Int)) (Maybe (Card, Int))
  | Field3 (Maybe (Card, Int)) (Maybe (Card, Int)) (Maybe (Card, Int))
data Player = Player Field Hand Score

-- During the game each player will do through three phases.
-- In each phase they will always have some common options, these
-- are encapsulated in the 'AlwaysOptions' type.
data AlwaysOptions = Quit | FinishTurn | Harvest Card | BuyBeanField
  deriving (Show, Read, Eq)

data PlantStatus = NothingPlanted | OnePlanted | TwoPlanted
  deriving (Show, Read, Eq)

data TurnStatus = InitialPlant | Discarding | MainTurn
  deriving (Show, Read, Eq)

data TurnStart = TS AlwaysOptions | TSPlant Card
  deriving (Show, Read, Eq)

data TurnDiscard = TD AlwaysOptions | Discard Card
  deriving (Show, Read, Eq)

data TurnMain = TM AlwaysOptions | TMPlant Card
  deriving (Show, Read, Eq)

-- Helper function to determine if a player can plant a given card.
-- Check if a.) there is a spare slot, and b.) in the case where there
-- is no spare slot, whether or not the card you're trying to plant
-- is the same as one that's already planted.
canPlant :: Field -> Card -> Bool
canPlant (Field2 mc1 mc2) c =
  let maybeCheck = do (c1, _) <- mc1
                      (c2, _) <- mc2
                      return (c `elem` [c1, c2])
  in case maybeCheck of
     Just b -> b
     Nothing -> True

canPlant (Field3 mc1 mc2 mc3) c =
  let maybeCheck = do (c1, _) <- mc1
                      (c2, _) <- mc2
                      (c3, _) <- mc3
                      return (c `elem` [c1, c2, c3])
  in case maybeCheck of
     Just b -> b
     Nothing -> True

-- Helper function to determine if a player can harvest a given field.
-- Remember that a field is a tuple of Maybe (Card, Int) values. So
-- when checking if we can harvest something, we need to compare against
-- only the Card part of the Maybe, and ignore the Int part.
canHarvest :: Field -> Card -> Bool
canHarvest (Field2 mc1 mc2) c =
  case cardList of
    [] -> False
    cs -> (Just c) `elem` cs
  where
    cardList = [mc1, mc2] |> filter (isJust) |> map (fmap fst)

canHarvest (Field3 mc1 mc2 mc3) c =
  case cardList of
    [] -> False
    cs -> (Just c) `elem` cs
  where
    cardList = [mc1, mc2, mc3] |> filter (isJust) |> map (fmap fst)

-- pc is the number of cards you've planted this turn.
validStartMove :: Player -> TurnStart -> PlantStatus -> Bool
validStartMove (Player field hand score) move plantStatus =
  case move of
    TS BuyBeanField -> score >= 3
    TS (Harvest c)  -> canHarvest field c
    TS FinishTurn   -> plantStatus /= NothingPlanted
    TS _            -> True
    TSPlant c       -> canPlant field c

validDiscardMove :: Player -> Bool
validDiscardMove (Player field hand score) = length hand > 0

validMainMove :: Player -> TurnMain -> Bool
validMainMove (Player field hand score) move =
  case move of
    TM BuyBeanField -> score >= 3
    TM (Harvest c)  -> canHarvest field c
    TM _            -> True
    TMPlant c       -> canPlant field c

-- Now we want to actually start the game proper.




















-- Way down here we start the actual making of the deck and playing of the game.
nCards :: Int
nCards = length baseDeck

makeFloats :: Int -> Int -> [Double]
makeFloats n seed = take n (randoms g)
  where g = mkStdGen seed

makeOrderedRange:: Int -> Int -> [Int]
makeOrderedRange n seed = map fst sortedFloats
  where baseFloats = zip [1..n] (makeFloats n seed)
        sortedFloats = baseFloats |> sortOn snd

makeDeck :: [a] -> Int -> [a]
makeDeck inputDeck seed = map snd orderedList
  where ordering = makeOrderedRange (length inputDeck) seed
        zippedList = zip ordering inputDeck
        orderedList = sortOn fst zippedList
