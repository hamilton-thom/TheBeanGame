

module Board
(
Player,
Board,
initialiseBoard,
initialisePlayer,
canHarvest,
harvest,
canPlant,
plant,
) where

import Cards
import qualified Field as Field
import Player
import Queue
import Shuffle
import TurnOrder

data Board = Board { players :: [Player],
                     deck :: Deck,
                     turnOrder :: TurnOrder }
  deriving Show

initialiseBoard :: [String] -> Int -> Board
initialiseBoard names seed =
  Board players startDeck turnOrder
  where
    players = map initialisePlayer names
    startDeck = Deck { current = fromList $ shuffle seed fullDeck,
                       discarded = emptyQueue }
    turnOrder = buildTurnOrder names

-- Requires the playerName to exist in the Board. Will have a runtime failure
-- otherwise!
splitPlayers :: Board -> String -> (Player, [Player])
splitPlayers (b @ Board { players = p:ps }) playerName =
  if name p == playerName then (p, ps)
  else
    let (resultPlayer, ps') = splitPlayers (b { players = ps }) playerName in
    (resultPlayer, p:ps')

getPlayer :: Board -> String -> Player
getPlayer board playerName = fst $ splitPlayers board playerName

canHarvest :: Board -> String -> Card -> Bool
canHarvest board playerName card =
  playerCanHarvest thisPlayer card
  where
    thisPlayer = getPlayer board playerName

-- Because "canHarvest" is exported, this function assumes that the player
-- can harvest the bean. Ideally, the player will only be given the option
-- to harvest one of the beans that they already have.
harvest :: Board -> String -> Card -> Board
harvest (b @ Board {players = ps, deck = d}) playerName card =
  b { players = updatedPlayers, deck = newDeck }
  where
    (thisPlayer, otherPlayers) = splitPlayers b playerName
    playerFields = fields thisPlayer
    currentCoins = coins thisPlayer
    currentDiscards = discarded d
    (newField, newCoins, newDiscards) = Field.harvestField playerFields card
    updatedPlayerCoins = thisPlayer { coins = currentCoins + newCoins,
                                      fields = newField }
    updatedPlayers = updatedPlayerCoins : otherPlayers
    newDeck = d { discarded = pushQueue newDiscards currentDiscards }

canPlant :: Board -> String -> Card -> Bool
canPlant board playerName card =
  playerCanPlant thisPlayer card
  where
    thisPlayer = getPlayer board playerName

-- Assumes that the player can plant the cards.
plant :: Board -> String -> Card -> Int -> Board
plant board playerName card n =
  board { players = updatedPlayer : otherPlayers }
  where
    (thisPlayer, otherPlayers) = splitPlayers board playerName
    playerField = fields thisPlayer
    plantedField = Field.plant playerField card n
    updatedPlayer = thisPlayer { fields = plantedField }
