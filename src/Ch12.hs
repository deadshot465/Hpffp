module Ch12 where

import Prelude
import Data.Char (isUpper, toLower)

notThe :: [Char] -> Maybe [Char]
notThe s =
  if lowerString /= "the" then Just s
  else Nothing
  where
    lowerString = toLower <$> s

replaceThe :: String -> String
replaceThe str = unwords $ (\s -> case notThe s of
  Nothing -> if isUpper $ head s then "A" else "a"
  Just s' -> s') <$> words str

vowels :: [Char]
vowels = ['a', 'e', 'i', 'o', 'u']

countTheBeforeVowel :: Num a => [Char] -> a
countTheBeforeVowel str =
  foldr (\s acc -> if head s `elem` vowels then acc + 1 else acc) 0 $ words $ toLower <$> str

countVowels :: (Foldable t, Num a, Functor t) => t Char -> a
countVowels str =
  foldr (\c acc -> if c `elem` vowels then acc + 1 else acc) 0 $ toLower <$> str

newtype Word'
  = Word' String deriving (Eq, Show)

mkWord :: [Char] -> Maybe Word'
mkWord str = if countVowels str > length str then Nothing else Just $ Word' str

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Num p => Nat -> p
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: (Ord t, Num t) => t -> Maybe Nat
integerToNat n | n < 0 = Nothing
               | otherwise = Just $ run n
  where
    run n' | n' == 0 = Zero
           | otherwise = Succ $ run (n' - 1)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee i f Nothing = i
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe i Nothing = i
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x : xs) = case x of
  Nothing -> catMaybes xs
  Just a -> a : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = run []
  where
    run acc [] = Just $ reverse acc
    run _ (Nothing : _) = Nothing
    run acc (Just x : xs) = run (x : acc) xs

lefts' :: Foldable t => t (Either a b) -> [a]
lefts' = foldr (\x acc -> case x of
  Left a -> a : acc
  Right _ -> acc) []

rights' :: Foldable t => t (Either a b) -> [b]
rights' = foldr (\x acc -> case x of
  Left _ -> acc
  Right a -> a : acc) []

partitionEithers' :: Foldable t => t (Either a b) -> ([a], [b])
partitionEithers' l = (lefts' l, rights' l)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right r) = Just $ f r

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left l) = f l
either' _ g (Right r) = g r

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
  Just (a, b) -> a : myUnfoldr f b
  Nothing -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b))