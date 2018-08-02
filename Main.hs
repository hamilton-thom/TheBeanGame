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

-- Each player has certain attributes that need to be kept track of
-- during the game.
type Score = Int
newtype Hand = Han [Card]

newtype Field = Field [Maybe (Card, Int)]
maxFields = 3

data Player = Player Field Hand Score PlantStatus

-- The remaining cards in the deck.
newtype Deck = Dec [Card]
-- The cards in the Middle.
newtype Middle = Mid [(Card, Int)]
-- The discard pile.
newtype Discard = Dis [Card]

data GlobalState = Global Deck Middle Discard [Player]

-- Helper function to determine if a player can plant a given card.
-- Check if a.) there is a spare slot, and b.) in the case where there
-- is no spare slot, whether or not the card you're trying to plant
-- is the same as one that's already planted.
canPlant :: Field -> Card -> Bool
canPlant (Field []) _     = False
canPlant (Field (x:xs)) c =
  case x of
    Nothing      -> True
    Just (c1, n) -> c1 == c || canPlant (Field xs) c

-- Helper function to determine if a player can harvest a given field.
-- Remember that a field is a tuple of Maybe (Card, Int) values. So
-- when checking if we can harvest something, we need to compare against
-- only the Card part of the Maybe, and ignore the Int part.
canHarvest :: Field -> Card -> Bool
canHarvest (Field []) _     = False
canHarvest (Field (x:xs)) c =
  case x of
    Just (c1, n) | c1 == c -> True
    Just (c2, n) | c2 /= c -> canHarvest (Field xs) c
    Nothing                -> canHarvest (Field xs) c

-- pc is the number of cards you've planted this turn.
validStartMove :: Player -> TurnStart -> Bool
validStartMove (Player (Field xs) hand score plantStatus) move =
  case move of
    TS BuyBeanField -> score >= 3 && length xs < maxFields
    TS (Harvest c)  -> canHarvest (Field xs) c
    TS FinishTurn   -> plantStatus /= NothingPlanted
    TS _            -> True
    TSPlant c       -> canPlant (Field xs) c

validDiscardMove :: Player -> Bool
validDiscardMove (Player field (Han hand) score turnStatus) = length hand > 0

validMainMove :: Player -> TurnMain -> Bool
validMainMove (Player (Field xs) hand score turnStatus) move =
  case move of
    TM BuyBeanField -> score >= 3 && length xs < maxFields
    TM (Harvest c)  -> canHarvest (Field xs) c
    TM _            -> True
    TMPlant c       -> canPlant (Field xs) c

-- Now we want to start the game proper.
-- Need to go through the mechanics of a turn.
-- You're given a set of cards, and then have several options
-- which will recursively run until you reach the end of your turn,
-- all the while you're changing the "World" which is largely just your
-- player result.

type PlayerID = Int

nextPlayer nPlayers playerID =
  if playerID == nPlayers then
    1
  else
    playerID + 1

type AllPlayers = [Player]

plant :: Card -> Field -> Field
plant c (Field xs) =
  if canPlant (Field xs) c == False
    then Field xs
    else Field (modified xs)
  where modified ns =
          case ns of
            []                          -> []
            (Just (c1, n)):cs | c1 == c -> (Just (c1, n + 1)):cs
            c1:cs                       -> c1 : (modified cs)

middleCards :: Middle -> [Card]
middleCards middle =
  case middle of
    Mid []          -> []
    Mid ((c, n):cs) -> c : (filter (/= c) (middleCards cs))

addCard :: Middle -> Card -> Middle
addCard middle c =
  if length matchingCards = 0 then
     (c, 1) : unmatchingCards
     else matchingCards ++ unmatchingCards

  where matchingCards = [(c', n+1) | (c', n) <- middle, c' == c]
        unmatchingCards = [(c', n) | (c', n) <- middle, c' /= c]

addCards :: Middle -> [Card] -> Middle
addCards m []     = m
addCards m (c:cs) = addCards (addCard m c) cs

-- Middle :: Mid [(Card, Int)]
-- This function needs improving so that we sort the discard pile.
matchDraw :: Discard -> Middle -> (Discard, Middle)
matchDraw discard middle =
  case discard of
    Dis [] -> (discard, middle)
    Dis (c:cs) -> if c `elem` midCards then
                    matchDraw (Dis cs) (addCard middle c)
                    else (discard, middle)
  where midCards = middleCards middle

data GlobalState = Global Deck Middle Discard [Player]

-- Draws n cards from the Deck, taking into account
-- that the deck might be exhausted, then returns the
-- resulting GlobalState and the cards which were drawn.
-- These are then appended to the middle, with all the
-- jiggery-pokery of the discard set-up done automatically.
safeDraw :: GlobalState -> Int -> (GlobalState, [Card])
safeDraw gs 0 = (gs, [])
safeDraw Global
          (Dec deck)
          middle
          (Dis discard)
         n =
  case deck of
    [] -> safeDraw Global (Dec []) middle (Dis start)


  where splitPos = length discard - 1
        start, [end] = splitAt splotPos discard



drawMain :: GlobalState -> GlobalState
drawMain (Global deck middle discard _) =

  safeDraw |> addCards |> matchDraw discard

-- draw and then resolve the matching
  where topThreeCards = safeDraw 3 deck

  if deckLen >= n then
    take n deck
    else take deckLen deck ++ drop (discardLen - cardDifference) discard
  where deckLen = length deck
        discardLen = length discard
        cardDifference = n - deckLen













{-
moveStart :: GlobalState ->   -- The global state of the game.
             PlayerID ->      -- The ID of the player making the move.
             TurnStart ->     -- The turn that they're trying to make.
             GlobalState      -- The returned global state.
moveStart gs pId ts =
  if validStart player ts then
     case ts of
     | TSPlant c -> plant c

  else gs


  where
    player = case gs of Global _ _ _ players -> players !! (pId - 1)
    plantStatus = case player of Player _ _ _ ps -> ps

-}








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

makeDeck :: Int -> [a] -> [a]
makeDeck seed inputDeck = map snd orderedList
  where ordering = makeOrderedRange (length inputDeck) seed
        zippedList = zip ordering inputDeck
        orderedList = sortOn fst zippedList
