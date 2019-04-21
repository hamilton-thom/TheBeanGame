

module Field
(
  Field,
  initialField,
  upgradeField,
  canPlant,
  plant
)

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

fieldContains :: Field -> Card -> Bool
fieldContains field card =
  n > 0
  where
    n = field |>
        fieldToList |>
        map (\p -> plotContains p card) |>
        filter id |>
        length

initialField :: Field
initialField = Field2 EmptyPlot EmptyPlot

upgradeField :: Field -> Field
upgradeField f =
  case f of
    Field3 _ _ _ -> f
    Field2 p1 p2 -> Field3 p1 p2 EmptyPlot

canPlant :: Field -> Card -> Bool
canPlant field card =
  hasEmptyPlot field || fieldContains field card

plantInPlot :: Plot -> Card -> Int -> Plot
plantInPlot EmptyPlot card n     = Plot (card, n)
plantInPlot (Plot (c, m)) card n = Plot (c, m + n)

plant :: Field -> Card -> Int -> Field
plant field card n =
  if fieldContains field card then
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
  else
    case field of
      Field2 EmptyPlot p2 -> Field2 (plantInPlot EmptyPlot card n) p2
      Field2 p1 EmptyPlot -> Field2 p1 (plantInPlot EmptyPlot card n)
      Field3 EmptyPlot p2 p3 -> Field3 (plantInPlot EmptyPlot card n) p2 p3
      Field3 p1 EmptyPlot p3 -> Field3 p1 (plantInPlot EmptyPlot card n) p3
      Field3 p1 p2 EmptyPlot -> Field3 p1 p2 (plantInPlot EmptyPlot card n)
