-- module for Trees
-- (c) 2016 Pascal Poizat
-- github: @pascalpoizat
--
-- This module is useless if Data.Tree supports different types in nodes and in leaves.
-- (this module is also used to learn Haskell by the way)
module Tree ( Tree(..)
            , isValid
            , leaves
            , depth)
where

import           Data.Bifunctor
import           Data.Set       (Set)

-- tree with values of type a in the leaves and values of type b in the nodes
data Tree a b
  = Leaf a
  | Node b [Tree a b]
  deriving (Show,Eq)

-- apply transformation f to the leaves of the tree and transformation g to its nodes
-- bimap :: (a -> a') -> (b -> b') -> Tree a b -> Tree a' b'
instance Bifunctor Tree where
  bimap f g (Leaf x)    = Leaf (f x)
  bimap f g (Node x ts) = Node (g x) (fmap (bimap f g) ts)

-- checks for the validity of a tree
isValid :: Tree a b -> Bool
isValid (Leaf _)    = True
isValid (Node _ ts) = not (null ts)

-- get the set of all leaves
leaves :: Ord a => Tree a b -> Set a
leaves (Leaf x)   = singleton x
leaves (Node _ ts) = unions (map leaves ts)

-- get the depth of the tree
depth :: Tree a b -> Int
depth (Leaf x)   = 1
depth (Node _ ts) = 1 + maximum (map depth ts)
