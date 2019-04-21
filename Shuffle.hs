

module Shuffle
(
shuffle
) where

import System.Random
import Data.List

-- Seed -> List -> ShuffledList
shuffle :: Int -> [a] -> [a]
shuffle _ [] = []
shuffle n xs =
  shuffleG g xs
  where
    g = mkStdGen n

shuffleG :: StdGen -> [a] -> [a]
shuffleG _ []  = []
shuffleG _ [x] = [x]
shuffleG g xs  =
  map fst $ sortOn (\(_, x) -> x) $ zip xs index
  where
    index = shuffleN g [1..(length xs)]

shuffleN :: StdGen -> [Int] -> [Int]
shuffleN _ []  = []
shuffleN _ [x] = [x]
shuffleN g xs  =
  let (g', x, xs') = splitRand g xs in
  x : shuffleN g' xs'

-- Will error on empty list input.
splitRand :: StdGen -> [a] -> (StdGen, a, [a])
splitRand g [x]    = (g, x, [])
splitRand g xs | length xs > 1 =
  (g', value, remainingValues)
  where
    (randomIndex, g') = randomR (0, length xs - 1) g
    value = xs !! randomIndex
    remainingValues =
      if randomIndex == 0
        then tail xs
        else take (randomIndex - 1) xs ++ drop randomIndex xs
