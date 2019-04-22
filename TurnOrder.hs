

module TurnOrder
(
  TurnOrder,
  playerNames,
  currentPlayer,
  nextTurn,
  buildTurnOrder
) where

import Player

data TurnOrder = TurnOrder { playerNames :: [String],
                             turnList :: [String],
                             currentPlayer :: String }

buildTurnOrder :: [String] -> TurnOrder
buildTurnOrder names =
  TurnOrder { playerNames = names,
              turnList = postpend names (head names),
              currentPlayer = head names }

postpend :: [a] -> a -> [a]
postpend [] a     = [a]
postpend (x:xs) a = x : postpend xs a

nextElement :: Eq a => [a] -> a -> a
nextElement (x:xs) y =
  if x == y then head xs else nextElement xs y

nextTurn :: TurnOrder -> TurnOrder
nextTurn t@(TurnOrder { turnList = tList, currentPlayer = c }) =
  t { currentPlayer = nextElement tList c}
