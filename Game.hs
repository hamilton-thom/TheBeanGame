

module Game
(
  TurnStage(..),
) where

import Board

  {-
  There are several stages to the game.

  Initially a player plays either one or two beans from their hand.
  Then they draw two cards from the deck
  We then follow a section of negotiation etc. which results in the planting
  of all the cards in the middle and any negotiated cards.
  After that has all been cleaned up we have to move on, the player draws two
  more cards which go to the end of their hand.
  It is now the next player's turn.
  After we cycle through the deck twice the final turn is had and then the
  game ends.
  -}

  -- Game ([Player], Deck)
  -- String = PlayerName

  playTurn :: Game -> Game

-- Just need to run through the stages of a turn for one player, then
-- can iterate the player to the next player and repeat.





data TurnStage = PlantHand | Negotiation | DrawCards

data Turn = Turn (TurnStage, String)

data InitialPlant = Plant1 Card | Plant2 Card Card

runTurn :: Game -> String -> TurnStage -> Game

runPlant :: Game -> String -> InitialPlant

type Offer = [Card]
