module Ch10 where

import Prelude

import Data.Time ( UTCTime (UTCTime), fromGregorian, secondsToDiffTime )
import Data.Foldable (Foldable(foldl'))
import Data.List (foldl1')

data DatabaseItem = DbString String
                  | DbNumber Integer 
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, World!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]
  
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = reverse . foldl' (\acc x -> case x of
  DbDate time -> time : acc
  _ -> acc) []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = reverse . foldl' (\acc x -> case x of
  DbNumber n -> n : acc
  _ -> acc) []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldl1' max . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = foldl' (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb items = sum / numberCount
  where
    sum = fromIntegral $ sumDb items :: Double
    numberCount = fromIntegral $ length $ filterDbNumber items :: Double

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

first20Fibs :: [Integer]
first20Fibs = take 20 fibs

lessThan100Fibs :: [Integer]
lessThan100Fibs = takeWhile (< 100) fibs

f :: [Integer]
f = scanl (*) 1 [1..]

factorial :: Int -> Integer
factorial n = take (n + 1) f !! n

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> acc || f x) False

myElem :: Eq a => a -> [a] -> Bool
myElem item = foldr (\x acc -> acc || item == x) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' item = any (== item)

myReverse :: [a] -> [a]
myReverse = foldl' (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred = foldr (\x acc -> if pred x then x : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . fmap f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldr1 (\x acc -> case f x acc of
  LT -> acc
  GT -> x
  EQ -> acc)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = foldr1 (\x acc -> case f x acc of
  LT -> x
  GT -> acc
  EQ -> acc)