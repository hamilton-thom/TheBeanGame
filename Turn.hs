

module Turn
(
  TurnStage(..),
) where

import Game

data TurnStage = PlantHand | Negotiation | DrawCards

data Turn = Turn (TurnStage, String)

data InitialPlant = Plant1 Card | Plant2 Card Card

runTurn :: Game -> String -> TurnStage -> Game

runPlant :: Game -> String -> InitialPlant

type Offer = [Card]
