module Cipher where

import Prelude

import Data.Char ( chr, ord )

ordA :: Int
ordA = ord 'A'

ordZ :: Int
ordZ = ord 'Z'

encrypt :: Int -> [Char] -> [Char]
encrypt shifts = map (\x -> chr (ord x + (shifts `mod` 26)))

buildCipherText :: [Char] -> [Char] -> [Char]
buildCipherText target source = run target source []
  where
    run _ [] acc = reverse acc
    run t@(x : xs) (y : ys) acc | y == ' ' = run t ys (' ' : acc)
                                | otherwise = run xs ys (x : acc)

vigenereEncrypt :: [Char] -> [Char] -> [Char]
vigenereEncrypt target source = map (\x ->
  let cipherCharacter = cipherText !! x in
  if cipherCharacter == ' ' then
    cipherCharacter
  else
    let shifted = ord (source !! x) + (ord cipherCharacter - ordA) in
    chr $ adjustShift shifted) [0..(sourceLength - 1)]
  where
    cipherText = buildCipherText (cycle target) source
    sourceLength = length source

    adjustShift n = if n > ordZ then
      ordA + (n `mod` ordZ) - 1
      else
        n