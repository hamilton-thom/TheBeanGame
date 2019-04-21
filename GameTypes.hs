

module Game
(
Player,
Game(..),
initialiseGame,
initialisePlayer,
canHarvest,
nextPlayer
) where

import CardTypes
import Field
import Player
import Queue
import Shuffle

data Game = Game ([Player], Deck)
  deriving Show

initialiseGame :: [String] -> Int -> Game
initialiseGame ps seed =
  Game (players, startDeck)
  where
    startDeck = Deck { current = fromList $ shuffle seed fullDeck,
                       discarded = emptyQueue }
    players = map initialisePlayer ps

-- Requires the playerName to exist in the game. Will have a runtime failure
-- otherwise!
splitPlayers :: Game -> String -> (Player, [Player])
splitPlayers (Game (p:ps, d)) playerName =
  if name p == playerName then (p, ps)
  else
    let (resultPlayer, ps') = getPlayer (Game ps, d) playerName in
    (resultPlayer, p:ps')

getPlayer :: Game -> String -> Player
getPlayer g playerName = fst $ splitPlayers g playerName

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



canHarvest :: Game -> String -> Card -> Bool
canHarvest g playerName card =
  playerCanHarvest thisPlayer card
  where
    thisPlayer = getPlayer g playerName

playerCanHarvest :: Player -> Card -> Bool
playerCanHarvest (p @ Player { fields = f }) c =
  case f of
    Field2 p1 p2 -> plotContains p1 c || plotContains p2 c
    Field3 p1 p2 p3 -> plotContains p1 c || plotContains p2 c || plotContains p3 c

-- Because "canHarvest" is exported, this function assumes that the player
-- can harvest the bean. Ideally, the player will only be given the option
-- to harvest one of the beans that they already have.
harvest :: Game -> String -> Card -> Game
harvest (g @ Game (ps, d)) playerName card =
case playerFields of
  Field2 f1@(Plot p1) f2@(Plot p2) ->
    if plotContains f1 card then
      let (newCoins, d') = harvestCards p1 in
      Game ((thisPlayer { coins = currentCoins + newCoins,
                          fields = Field2 EmptyPlot f2 }) : otherPlayers, pushQueue d' d)
    else
      let (newCoins, d') = harvestCards p2 in
      Game (thisPlayer { coins = currentCoins + newCoins,
                          fields = Field2 f1 EmptyPlot }) : otherPlayers, pushQueue d' d)

  Field3 f1@(Plot p1) f2@(Plot p2) f3@(Plot p3) ->
    if plotContains f1 card then
      let (newCoins, d') = harvestCards p1 in
      Game ((thisPlayer { coins = currentCoins + newCoins,
                          fields = Field3 EmptyPlot f2 f3 }) : otherPlayers, pushQueue d' d)
    else if plotContains f2 card then
      let (newCoins, d') = harvestCards p2 in
      Game (thisPlayer { coins = currentCoins + newCoins,
                          fields = Field2 f1 EmptyPlot f3 }) : otherPlayers, pushQueue d' d)
    else
      let (newCoins, d') = harvestCards p3 in
      Game (thisPlayer { coins = currentCoins + newCoins,
                          fields = Field2 f1 f2 EmptyPlot }) : otherPlayers, pushQueue d' d)
  where
    (thisPlayer, otherPlayers) = splitPlayers g playerName
    playerFields = fields thisPlayer
    currentCoins = coins thisPlayer

-- Assumes that the player can plant the cards.
plantCards :: Game -> String -> (Card, Int)
plantCards (g @ Game (ps, d)) playerName (c, n) =



  where
    (thisPlayer, otherPlayers) = splitPlayers g playerName
    playerFields = fields thisPlayer
