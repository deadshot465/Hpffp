module Main where

import Prelude

import qualified Ch10
import qualified Ch11
import qualified Ch12
import qualified Cipher
import Data.Char (chr, ord)
import qualified HuttonsRazor
import qualified Phone

readNumber :: IO Integer
readNumber = do
  getLine >>= \s -> pure (read s :: Integer)

runCh10 :: IO ()
runCh10 = do
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

runCh11 :: IO ()
runCh11 = do
  print Ch11.allProgrammers
  let t1 = Ch11.insert' 0 Ch11.Leaf
  let t2 = Ch11.insert' 3 t1
  let t3 = Ch11.insert' 5 t2
  print t1
  print t2
  print t3
  Ch11.mapOkay
  Ch11.testPreorder
  Ch11.testInorder
  Ch11.testPostorder
  print $ Cipher.encrypt 27 "ABCD"
  print $ Cipher.vigenereEncrypt "ALLY" "MEET AT DAWN"
  print $ Ch11.isSubseqOf "blah" "blahwoot"
  print $ Ch11.isSubseqOf "blah" "wootblah"
  print $ Ch11.isSubseqOf "blah" "wboloath"
  print $ Ch11.isSubseqOf "blah" "wootbla"
  print $ Ch11.isSubseqOf "blah" "halbwoot"
  print $ Ch11.isSubseqOf "blah" "blawhoot"
  print $ Ch11.capitalizeWords "hello world"
  print $ Ch11.capitalizeWord "Chortle"
  print $ Ch11.capitalizeWord "chortle"
  print $ Ch11.capitalizeParagraph "blah. woot ha."
  print Phone.convertedConvo
  print $ Phone.cellPhonesDead Phone.daPhone "The phone has run out of battery"
  print $ Phone.fingerTaps <$> Phone.convertedConvo
  print $ Phone.mostPopularLetter <$> Phone.convo
  print $ Phone.coolestLtr Phone.convo
  print $ Phone.coolestWord Phone.convo
  print $ HuttonsRazor.eval (HuttonsRazor.Add (HuttonsRazor.Lit 1) (HuttonsRazor.Lit 9001))
  print $ HuttonsRazor.printExpr (HuttonsRazor.Add (HuttonsRazor.Lit 1) (HuttonsRazor.Lit 9001))
  let a1 = HuttonsRazor.Add (HuttonsRazor.Lit 9001) (HuttonsRazor.Lit 1)
  let a2 = HuttonsRazor.Add a1 (HuttonsRazor.Lit 20001)
  let a3 = HuttonsRazor.Add (HuttonsRazor.Lit 1) a2
  print $ HuttonsRazor.printExpr a3
  pure ()

runCh12 :: IO ()
runCh12 = do
  print $ Ch12.replaceThe "the cow loves us"
  print $ Ch12.countTheBeforeVowel "the cow"
  print $ Ch12.countTheBeforeVowel "the evil cow"
  print $ Ch12.countVowels "the cow"
  print $ Ch12.countVowels "Mikolajczak"
  print $ Ch12.natToInteger Ch12.Zero
  print $ Ch12.natToInteger (Ch12.Succ Ch12.Zero)
  print $ Ch12.natToInteger (Ch12.Succ $ Ch12.Succ Ch12.Zero)
  print $ Ch12.integerToNat 0
  print $ Ch12.integerToNat 1
  print $ Ch12.integerToNat 2
  print $ Ch12.integerToNat (-1)
  print $ Ch12.catMaybes [Just 1, Nothing, Just 2]
  print $ Ch12.catMaybes (replicate 3 Nothing :: [Maybe ()])
  print $ Ch12.flipMaybe [Just 1, Just 2, Just 3]
  print $ Ch12.flipMaybe [Just 1, Nothing, Just 3]
  print $ Ch12.lefts' [Left 1, Left 2, Right 3, Left 4, Right 5, Right 6, Left 7]
  print $ Ch12.rights' [Left 1, Left 2, Right 3, Left 4, Right 5, Right 6, Left 7]
  print $ take 10 $ Ch12.myIterate (+ 1) 0
  print $ take 10 $ Ch12.myUnfoldr (\b -> Just (b, b + 1)) 0
  print $ take 10 $ Ch12.betterIterate (+ 1) 0
  print $ Ch12.treeBuild 0
  print $ Ch12.treeBuild 1
  print $ Ch12.treeBuild 2
  print $ Ch12.treeBuild 3
  pure ()

main :: IO ()
main = do
  runCh12