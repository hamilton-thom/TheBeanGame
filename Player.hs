

module Player
(

) where

data Player = Player { name :: String,
                       coins :: Int,
                       hand :: Queue Card,
                       fields :: Field,
                       tradingArea :: [Card]}
  deriving Show

initialisePlayer :: String -> Player
initialisePlayer s = Player { name = s,
                              coins = 0,
                              hand = emptyQueue,
                              fields = Field2 EmptyPlot EmptyPlot,
                              tradingArea = [] }

findPlayer :: [Player] -> String -> Player
findPlayer (p:ps) player = if name p == player then p else findPlayer ps player

playerCanPlant :: Player -> Card -> Bool
playerCanPlant player card =
  case f of
    Field2 p1 p2 ->
      p1 == EmptyPlot ||
      p2 == EmptyPlot ||
      plotContains p1 card ||
      plotContains p2 card
    Field3 p1 p2 p3 ->
      p1 == EmptyPlot ||
      p2 == EmptyPlot ||
      p3 == EmptyPlot ||
      plotContains p1 card ||
      plotContains p2 card ||
      plotContains p3 card

  where
    f = fields p
