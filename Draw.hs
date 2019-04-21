
module Draw
(
drawCards
) where

import GameTypes
import Queue
import Shuffle
import Utilities

drawCards :: Game -> String -> Game
drawCards (Game (ps, d)) playerName =
  Game (ps', newDeck)
  where
    (newCards, newDeck) = draw2 d
    mapCards :: Player -> Bool = (\p -> if name p == playerName
                                  then addCards p newCards
                                  else p)
    ps' = map mapCards ps

addCards :: Player -> Queue Card -> Player
addCards (p @ Player { hand = qh }) qc = p { hand = pushQueue qc qh }

draw2 :: Deck -> (Queue Card, Deck)
draw2 (d @ Deck { current = qCurrent,
                  discarded = qDiscarded }) =
  case size qCurrent of
    | 0 ->
      let (Just topQ, qRemaining) = popN 2 qDiscardShuffled in
      (topQ,
       d { current = qRemaining,
           discarded = emptyQueue }
           
    | 1 ->
      let (Just topQ, qDiscardShuffled') = pop 1 qDiscardShuffled;
          (Just topCurrent, _) = pop qCurrent in
      (emptyQueue |> push topCurrent |> pushQueue topQ,
       d { current = qDiscardShuffled',
           discarded = emptyQueue })

    | otherwise ->
      let (Just topQ, qCurrent') = popN 2 qCurrent in
      (topQ,
       d { current = qCurrent'})
    where
      seed = 1
      qDiscardShuffled = qDiscarded |> toList |> shuffle seed |> fromList
