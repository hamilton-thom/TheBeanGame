-- This file will give the mechanics behind the Bean game. The ultimate objective
-- of this project is to develop an AI for the Bean game in order to determine
-- optimal strategies.

import Data.List

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

(>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>) f g = g . f

data Card
  = Cocoa
  | Garden
  | Red
  | BlackEyed
  | Soy
  | Green
  | StinkyG
  | Chili
  | Blue
  | Wax
  | Coffee

beanCounts :: [Card Int]
beanCounts = [Cocoa, 4; Garden, 6;, Red, 8;, BlackEyed, 10; Soy, 12;
              Green, 14; StinkyG, 16; Chili, 18; Blue, 20; Wax, 22; Coffee, 24]

data Deck = [Card]
data Hand = [Card Int]
data Field =
  | Field2 (Maybe Card) (Maybe Card)
  | Field3 (Maybe Card) (Maybe Card) (Maybe Card)
data Player = Player Field Hand Int

-- During the game each player will do through three phases.
-- In each phase they will always have some common options, these
-- are encapsulated in the 'AlwaysOptions' type.
data AlwaysOptions = Quit | FinishTurn | Harvest Card | BuyBeanField
  deriving Show, Read

data PlantStatus = NothingPlanted | OnePlanted | TwoPlanted
data TurnStatus = InitialPlant | Discard | MainTurn

data TurnStart = TS AlwaysOptions | TSPlant Card
  deriving Show, Read

data TurnDiscard = TD AlwaysOptions | Discard Card
  deriving Show, Read

data TurnMain = TM AlwaysOptions | TMPlant Card
  deriving Show, Read

-- Helper function to determine if a player can plant a given card.
canPlant :: Field -> Card -> Bool
canPlant (Field2 mc1 mc2) c =
  let maybeCheck = do c1 <- mc1
                      c2 <- mc2
                      return (c `elem` [c1; c2])
  in case maybeCheck of
     | Maybe b -> b
     | Nothing -> True

canPlant (Field2 mc1 mc2 mc3) c =
  let maybeCheck = do c1 <- mc1
                      c2 <- mc2
                      c3 <- mc3
                      return (c `elem` [c1; c2; c3])
  in case maybeCheck of
     | Maybe b -> b
     | Nothing -> True

-- Helper function to determine if a player can harvest a given field.
canHarvest :: Field -> Card -> Bool
canHarvest (Field mc1 mc2) c =
  cardList = [mc1; mc2] |> filter (isJust)
  case cardList of
    | [] -> False
    | cs -> c `elem` cs

canHarvest (Field mc1 mc2 mc3) c =
  cardList = [mc1; mc2; mc3] |> filter (isJust)
  case cardList of
    | [] -> False
    | cs -> c `elem` cs

-- pc is the number of cards you've planted this turn.
validStartMove :: Player -> TurnStart -> PlantStatus -> Bool
validStart (Player field hand score) move plantStatus =
  case move of
    | TS BuyBeanField -> score >= 3
    | TS (Harvest c)  -> canHarvest field c
    | TS FinishTurn   -> plantStatus /= NothingPlanted
    | TS _            -> True
    | TSPlant c       -> canPlant field c

validDiscardMove :: Player -> Bool
validDiscardMove (Player field hand score) = length hand > 0

validMainMove :: Player -> TurnMain -> Bool
validMainMove (Player field hand score) move =
  case move of
    | TM BuyBeanField -> score >= 3
    | TM (Harvest c)  -> canHarvest field c
    | TM _            -> True
    | TMPlant c       -> canPlant field c

-- Now we want to actually start the game proper.




















-- Way down here we start the actual making of the deck and playing of the game.

nCards = beanCounts |> map snd |> sum

randomGen = mkStdGen 1

makeFloats :: Int -> [Double]
makeFloats n = replicateM n (randomIO :: IO Double)

makeOrderedRange:: Int -> [Int]
makeOrderedRange n =
  where baseFloats = makeFloats n
        sortedFloats = zip [1..n] (sort baseFloats)




makeDeck n =
    let rGen = mkStdGen 1 in
    where
      ns = [1..n]
      extract i ns = (ns !! (i-1), take (i-1) ns ++ drop i ns)
      pickRand = map (\x -> (randomR (0, x-1) rGen)) (reverse [0..x-1])
