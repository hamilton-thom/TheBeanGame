

module GameTypes
(
Player,
Game(..),
TurnStage(..),
initialiseGame,
initialisePlayer,
nextPlayer
) where

import CardTypes
import Queue
import Shuffle

data Plot = Plot (Card, Int) | EmptyPlot
  deriving Show

data Field = Field2 Plot Plot | Field3 Plot Plot Plot
  deriving Show

data Player = Player { name :: String,
                       coins :: Int,
                       hand :: Queue Card,
                       fields :: Field }
  deriving Show

data Game = Game ([Player], Deck)
  deriving Show

data TurnStage = PlantHand | Negotiation | DrawCards

data InitialPlant = Plant1 Card | Plant2 Card Card

initialiseGame :: [String] -> Int -> Game
initialiseGame ps n =
  Game (players, startDeck)
  where
    startDeck = shuffle n fullDeck
    players = map initialisePlayer ps

initialisePlayer :: String -> Player
initialisePlayer s = Player { name = s,
                              coins = 0,
                              hand = emptyQueue,
                              fields = Field2 EmptyPlot EmptyPlot}

nextPlayer :: Game -> String -> String
nextPlayer (Game (ps, _)) currentPlayer =
  let ps' = postpend ps (head ps) in
  nextItem ps' currentPlayer

postpend :: [a] -> a -> [a]
postpend [] a     = [a]
postpend (x:xs) a = x : postpend xs a

nextItem :: Eq a => [a] -> a -> a
nextItem (x:xs) y =
  if x == y then head xs else nextItem xs y

harvest :: Game -> String -> Card -> Game
harvest (Game (ps, d)) player card =


  where
    thisPlayer = findPlayer ps player


findPlayer :: [Player] -> String -> Player
findPlayer (p:ps) player = if name p == player then p else findPlayer ps player
