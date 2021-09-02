{-# LANGUAGE PartialTypeSignatures #-}
module Phone where

import Prelude
import Data.Char (isUpper, toLower)
import Data.Maybe (fromMaybe)
import Data.List (elemIndex, maximumBy, nub)

newtype DaPhone = DaPhone [String]

type Digit = Char
type Presses = Int

validButtons :: [Digit]
validButtons = "1234567890*#"

validateButton :: Digit -> Bool
validateButton = flip elem validButtons

layout :: [String]
layout =
  [ "1"
  , "abc2"
  , "def3"
  , "ghi4"
  , "jkl5"
  , "mno6"
  , "pqrs7"
  , "tuv8"
  , "wxyz9"
  , "*"
  , "+ 0"
  , ".,#"
  ]

daPhone :: DaPhone
daPhone = DaPhone layout

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol OK. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "OK. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

findLetter :: [String] -> Char -> (Char, Int)
findLetter [] _ = (' ', -1)
findLetter (x : xs) char | char `elem` x = (last x, fromMaybe 0 (elemIndex char x) + 1)
                                     | otherwise = findLetter xs char

reverseTaps :: DaPhone -> Digit -> [(Digit, Presses)]
reverseTaps (DaPhone l) char =
  if isUpper char then ('*', 1) : [findLetter l (toLower char)]
  else [findLetter l char]

convertedConvo :: [[(Digit, Presses)]]
convertedConvo = concatMap (reverseTaps daPhone) <$> convo

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p msg = concat $ reverseTaps p <$> msg

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, presses) -> (+ presses)) 0

countLetters :: Eq a => [a] -> a -> (a, Int)
countLetters s c = (c, length $ filter (== c) s)

mostPopularLetter :: String -> Char
mostPopularLetter s =
  dedupAndGetMax $ countLetters s
  <$> (toLower <$> filter (/= ' ') s)

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: Foldable t => t String -> String
coolestWord s =
  dedupAndGetMax $ countLetters lowerWords <$> lowerWords
  where
    lowerWords = fmap toLower <$> concatMap words s

dedupAndGetMax :: Eq a => [(a, Int)] -> a
dedupAndGetMax = fst . maximumBy (\(_, a) (_, b) -> compare a b) . nub