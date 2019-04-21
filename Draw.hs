
module Draw
(
drawCards
) where

import GameTypes
import Queue
import Shuffle

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
draw2 (Deck { current = qCurrent, trashed = qTrashed, discarded = qDiscarded })
  | size qCurrent == 0 =
    (top2, Deck { current = remaining, trashed = t, discarded = [] }
    where
      shuffledDiscard = shuffle 1 d
      top2 = take 2 shuffleDiscard
      remaining = drop 2 shuffleDiscard
      
  | size qCurrent == 1 =
    ([head xs, new], Deck { current = shuffledDiscard, trashed = t, discarded = [] }
    where
      shuffledDiscard = shuffle 1 d
      new = head shuffledDiscard
      remaining = drop 1 shuffleDiscard

  | otherwise =
    (take 2 xs, Deck { current = drop 2 xs, trashed = t, discarded = d })
