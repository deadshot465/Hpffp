module Main where

import Prelude

import Lib
import qualified Ch10

main :: IO ()
main = do
  print $ Ch10.mostRecent Ch10.theDatabase
  print Ch10.first20Fibs
  print Ch10.lessThan100Fibs
  print $ Ch10.factorial 9
  print $ Ch10.myAny even [1, 3, 5]
  print $ Ch10.myAny odd [1, 3, 5]
  print $ Ch10.myElem 1 [1..10]
  print $ Ch10.myElem' 1 [2..10]
  print $ Ch10.myReverse "blah"
  print $ Ch10.myReverse [1..5]
  print $ Ch10.myMap (* 2) [1..10]
  print $ Ch10.myFilter even [1..10]
  print $ Ch10.squish [[1, 2], [3, 4, 5], [6, 7, 8, 9]]
  print $ Ch10.squishMap (\x -> [1, x, 3]) [2]
  print $ Ch10.squishMap (\x -> "WO " ++ [x] ++ " OT ") "blah"
  print $ Ch10.squishAgain [[1, 2], [3, 4, 5], [6, 7, 8, 9]]
  print $ Ch10.myMaximumBy (\_ _ -> GT) [1..10]
  print $ Ch10.myMaximumBy (\_ _ -> LT) [1..10]
  print $ Ch10.myMaximumBy compare [1..10]
  print $ Ch10.myMinimumBy (\_ _ -> GT) [1..10]
  print $ Ch10.myMinimumBy (\_ _ -> LT) [1..10]
  print $ Ch10.myMinimumBy compare [1..10]