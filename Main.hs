-- This file will give the mechanics behind the Bean game. The ultimate objective
-- of this project is to develop an AI for the Bean game in order to determine
-- optimal strategies.

import Data.Maybe
import Control.Monad (replicateM)

import Utilities
import Types





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
  if length matchingCards == 0 then
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
          players
         n =
           ((Global (Dec remainingDeckCards) middle (Dis discard) players),
           (deckDrawnCards ++ discardDrawnCards))
  where nDeckCardsDrawn = min n (length deck)
        deckDrawnCards = take nDeckCardsDrawn deck
        remainingDeckCards = drop nDeckCardsDrawn deck
        shuffledDiscard = makeDeck 1 discard
        nDiscardCardsDrawn = n - nDeckCardsDrawn
        discardDrawnCards = take nDiscardCardsDrawn shuffledDiscard

drawMain :: GlobalState -> GlobalState
drawMain (Global deck middle discard players) =
  Global nDeck resultMiddle resultDiscard nPlayers
  where ((Global nDeck nMid nDiscard nPlayers), topThreeCards) =
          safeDraw (Global deck middle discard players) 3
        (resultDiscard, resultMiddle) = matchDraw nDiscard nMid
