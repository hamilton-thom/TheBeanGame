
module Utilities ((|>), makeDeck) where

import System.Random
import Data.List (sortOn)


(|>) :: a -> (a -> b) -> b
(|>) x f = f x

makeFloats :: Int -> Int -> [Double]
makeFloats n seed = take n (randoms g)
  where g = mkStdGen seed

makeOrderedRange:: Int -> Int -> [Int]
makeOrderedRange n seed = map fst sortedFloats
  where baseFloats = zip [1..n] (makeFloats n seed)
        sortedFloats = baseFloats |> sortOn snd

makeDeck :: Int -> [a] -> [a]
makeDeck seed inputDeck = map snd orderedList
  where ordering = makeOrderedRange (length inputDeck) seed
        zippedList = zip ordering inputDeck
        orderedList = sortOn fst zippedList
