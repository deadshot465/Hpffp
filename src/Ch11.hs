{-# LANGUAGE NamedFieldPuns #-}
module Ch11 where

import Prelude

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer
  { os :: OperatingSystem
  , lang :: ProgLang
  } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allProgLangs :: [ProgLang]
allProgLangs =
  [ Haskell
  , Agda
  , Idris
  , PureScript
  ]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer { os = o, lang = p } | o <- allOperatingSystems, p <- allProgLangs ]

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
       1
       (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected =
  Node (Node Leaf 4 Leaf)
       2
       (Node Leaf 5 Leaf)

mapOkay :: IO ()
mapOkay =
  if mapTree (+ 1) testTree' == mapExpected
    then print "yup, OK!"
    else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = (a : preorder left) ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = preorder left ++ (a : preorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = reverse $ a : (preorder right ++ preorder left)

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
       2
       (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad news bears."

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f acc = foldr f acc . preorder