
module Card
( Card,
  Deck(..),
  harvestCards,
  fullDeck
) where

import Queue

data Card =
    Coffee
  | Wax
  | Blue
  | Chili
  | Stink
  | Green
  | Soy
  | BlackEyed
  | Red
  | Garden
  | Cocoa
  deriving (Show, Eq, Enum, Bounded)

cardsInDeck :: [(Card, Int)]
cardsInDeck = zip [Coffee .. Cocoa] (reverse [2 * n | n <- [2..12]])

data Deck = Deck { current :: Queue Card, discarded :: Queue Card }
  deriving Show

fullDeck :: [Card]
fullDeck = concat [replicate n c | (c, n) <- cardsInDeck]

getStats :: Card -> [Maybe Int]
getStats card =
  case card of
    Coffee    -> [Just 4, Just 7, Just 10, Just 12}
  | Wax       -> [Just 4, Just 7, Just 9, Just 11}
  | Blue      -> [Just 4, Just 6, Just 8, Just 10}
  | Chili     -> [Just 3, Just 6, Just 8, Just 9}
  | Stink     -> [Just 3, Just 5, Just 7, Just 8}
  | Green     -> [Just 3, Just 5, Just 6, Just 7}
  | Soy       -> [Just 2, Just 4, Just 6, Just 7}
  | BlackEyed -> [Just 2, Just 4, Just 5, Just 6}
  | Red       -> [Just 2, Just 3, Just 4, Just 5}
  | Garden    -> [Nothing, Just 2, Just 3, Nothing}
  | Cocoa     -> [Nothing, Just 2, Just 3, Just 12}


-- When you harvest a card you get coins for that card,
-- the coin cards are essentially trashed, the remaining
-- cards are pushed through to the discard pile.
-- This function is uncurried as it is applied to "Plot (Coin, Count)"
-- types later on.
harvestCoinCount :: (Card, Int) -> Int
harvestCoinCount (c, n) =
  case c of
    Garden -> case coins of
               1 -> 0
               4 -> 3
               _ -> coins
    Cocoa  -> case coins of
               1 -> 0
               _ -> coins
    _      -> coins
  where
    stats = getStats c
    f Nothing  = True
    f (Just m) = m >= n
    coins = length $ filter id $ map f hAll

harvestCards :: (Card, Int) -> (Int, Queue Card)
harvestCards (c, n) =
  (coins, fromList $ replicate (n - coins) c)
  where
    coins = harvestCoinCount (c, n)
