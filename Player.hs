

module Player
(
  Player,
  initialisePlayer,
  playerCanPlant,
  playerCanHarvest
) where

import Field

data Player = Player { name :: String,
                       coins :: Int,
                       hand :: Queue Card,
                       fields :: Field,
                       tradingArea :: [Card]}
  deriving (Show, Eq)

initialisePlayer :: String -> Player
initialisePlayer s = Player { name = s,
                              coins = 0,
                              hand = emptyQueue,
                              fields = Field2 EmptyPlot EmptyPlot,
                              tradingArea = [] }

playerCanPlant :: Player -> Card -> Bool
playerCanPlant (Player { fields = f }) card = canPlant f card

playerCanHarvest :: Player -> Card -> Bool
playerCanHarvest (Player { fields = f }) card = canHarvest f card
