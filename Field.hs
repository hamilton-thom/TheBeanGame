

module Field
(
  Field,
  initialField,
  upgradeField,
  canHarvest,
  harvestField,
  canPlant,
  plant
) where

import Cards
import Queue
import Utilities

data Plot = Plot (Card, Int) | EmptyPlot
  deriving (Show, Eq)

data Field = Field2 Plot Plot | Field3 Plot Plot Plot
  deriving (Show, Eq)

plotContains :: Plot -> Card -> Bool
plotContains EmptyPlot card            = False
plotContains (Plot (plotCard, _)) card = plotCard == card

hasEmptyPlot :: Field -> Bool
hasEmptyPlot (Field2 p1 p2)    = p1 == EmptyPlot || p2 == EmptyPlot
hasEmptyPlot (Field3 p1 p2 p3) = p1 == EmptyPlot || p2 == EmptyPlot || p3 == EmptyPlot

fieldToList :: Field -> [Plot]
fieldToList field =
  case field of
    Field2 p1 p2 -> [p1, p2]
    Field3 p1 p2 p3 -> [p1, p2, p3]

canHarvest :: Field -> Card -> Bool
canHarvest field card =
  n > 0
  where
    n = field |>
        fieldToList |>
        map (\p -> plotContains p card) |>
        filter id |>
        length

initialField :: Field
initialField = Field2 EmptyPlot EmptyPlot

upgradeField :: Field -> Int -> (Field, Int)
upgradeField f coins =
  case f of
    Field3 _ _ _ -> (f, coins)
    Field2 p1 p2 -> (Field3 p1 p2 EmptyPlot, coins - 3)

canPlant :: Field -> Card -> Bool
canPlant field card =
  hasEmptyPlot field || canHarvest field card

plantInPlot :: Plot -> Card -> Int -> Plot
plantInPlot EmptyPlot card n     = Plot (card, n)
plantInPlot (Plot (c, m)) card n = Plot (c, m + n)

plant :: Field -> Card -> Int -> Field
plant field card n =
  if canHarvest field card then
    case field of
      Field2 p1 p2 ->
        if plotContains p1 card then
          Field2 (plantInPlot p1 card n) p2
          else Field2 p1 (plantInPlot p2 card n)
      Field3 p1 p2 p3 ->
        if plotContains p1 card then
          Field3 (plantInPlot p1 card n) p2 p3
          else if plotContains p2 card then
             Field3 p1 (plantInPlot p2 card n) p3
             else Field3 p1 p2 (plantInPlot p3 card n)
  else -- Field doesn't already contain the plot, but has an empty plot.
    case field of
      Field2 EmptyPlot p2 -> Field2 (plantInPlot EmptyPlot card n) p2
      Field2 p1 EmptyPlot -> Field2 p1 (plantInPlot EmptyPlot card n)
      Field3 EmptyPlot p2 p3 -> Field3 (plantInPlot EmptyPlot card n) p2 p3
      Field3 p1 EmptyPlot p3 -> Field3 p1 (plantInPlot EmptyPlot card n) p3
      Field3 p1 p2 EmptyPlot -> Field3 p1 p2 (plantInPlot EmptyPlot card n)


-- Returns the resultant field, the number of coins gained
-- and the cards which need to be added to the end of the
-- discard pile.
harvestField :: Field -> Card -> (Field, Int, Queue Card)
harvestField field card =
  case field of
    Field2 f1@(Plot p1) f2@(Plot p2) ->
      if plotContains f1 card then
        let (newCoins, discards) = harvestCards p1 in
        (Field2 EmptyPlot f2, newCoins, discards)
      else
        let (newCoins, discards) = harvestCards p2 in
        (Field2 f1 EmptyPlot, newCoins, discards)

    Field3 f1@(Plot p1) f2@(Plot p2) f3@(Plot p3) ->
      if plotContains f1 card then
        let (newCoins, discards) = harvestCards p1 in
        (Field3 EmptyPlot f2 f3, newCoins, discards)
      else if plotContains f2 card then
        let (newCoins, discards) = harvestCards p2 in
        (Field3 f1 EmptyPlot f3, newCoins, discards)
      else
        let (newCoins, discards) = harvestCards p3 in
        (Field3 f1 f2 EmptyPlot, newCoins, discards)
