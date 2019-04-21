
module CardTypes
( Card,
  Deck(..),
  Stats,
  getStats,
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

data Deck = Deck { current :: Queue Card, trashed :: Queue Card, discarded :: Queue Card }
  deriving Show

fullDeck :: Deck
fullDeck = Deck { current = (concat [replicate n c | (c, n) <- cardsInDeck]),
                  trashed = [],
                  discarded = [] }

data Stats = Stats { harvest1 :: Maybe Int,
                     harvest2 :: Maybe Int,
                     harevst3 :: Maybe Int,
                     harvest4 :: Maybe Int}

getStats :: Card -> Stats
getStats c =
  case c of
    Coffee    -> Stats { harvest1 = Just 4, harvest2 = Just 7, harvest3 = Just 10, harvest4 = Just 12}
  | Wax       -> Stats { harvest1 = Just 4, harvest2 = Just 7, harvest3 = Just 9, harvest4 = Just 11}
  | Blue      -> Stats { harvest1 = Just 4, harvest2 = Just 6, harvest3 = Just 8, harvest4 = Just 10}
  | Chili     -> Stats { harvest1 = Just 3, harvest2 = Just 6, harvest3 = Just 8, harvest4 = Just 9}
  | Stink     -> Stats { harvest1 = Just 3, harvest2 = Just 5, harvest3 = Just 7, harvest4 = Just 8}
  | Green     -> Stats { harvest1 = Just 3, harvest2 = Just 5, harvest3 = Just 6, harvest4 = Just 7}
  | Soy       -> Stats { harvest1 = Just 2, harvest2 = Just 4, harvest3 = Just 6, harvest4 = Just 7}
  | BlackEyed -> Stats { harvest1 = Just 2, harvest2 = Just 4, harvest3 = Just 5, harvest4 = Just 6}
  | Red       -> Stats { harvest1 = Just 2, harvest2 = Just 3, harvest3 = Just 4, harvest4 = Just 5}
  | Garden    -> Stats { harvest1 = Nothing, harvest2 = Just 2, harvest3 = Just 3, harvest4 = Nothing}
  | Cocoa     -> Stats { harvest1 = Nothing, harvest2 = Just 2, harvest3 = Just 3, harvest4 = Just 12}
