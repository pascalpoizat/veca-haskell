-- module for Trees
-- (c) 2016 Pascal Poizat
-- github: pascal.poizat
--
-- This module is useless if Data.Tree supports different types in nodes and in leaves.
-- (this module is also used to learn Haskell by the way)
module Tree ( Tree(..)
            , isValid
            , treeleafmap
            , treenodemap
            , treemap
            , treeleaves
            , treedepth)
where

import Data.Set as S hiding (null, map)

-- tree with values of type a in the leaves and values of type b in the nodes
data Tree a b
  = Leaf a
  | Node b [Tree a b]
  deriving Show

-- checks for the validity of a tree
isValid :: Tree a b -> Bool
isValid (Leaf _)   = True
isValid (Node _ t) = not (null t)

-- apply transformation f to the values in the leaves of a tree
treeleafmap :: (a -> a') -> Tree a b -> Tree a' b
treeleafmap f (Leaf x)   = Leaf (f x)
treeleafmap f (Node x t) = Node x (map (treeleafmap f) t)

-- apply transformation f to the values in the nodes of a tree
treenodemap :: (b -> b') -> Tree a b -> Tree a b'
treenodemap f (Leaf x)   = Leaf x
treenodemap f (Node x t) = Node (f x) (map (treenodemap f) t)

-- apply transformation f1 to the values in the leaves of a tree
-- and transformation f2 to the values in the nodes of the tree
treemap :: (a -> a') -> (b -> b') -> Tree a b -> Tree a' b'
treemap f1 f2 (Leaf x)   = Leaf (f1 x)
treemap f1 f2 (Node x t) = Node (f2 x) (map (treemap f1 f2) t)

-- get the set of all leaves
treeleaves :: Ord a => Tree a b -> S.Set a
treeleaves (Leaf x)   = singleton x
treeleaves (Node _ t) = unions (map treeleaves t)

-- get the depth of the tree
treedepth :: Tree a b -> Int
treedepth (Leaf x) = 1
treedepth (Node _ t) = 1 + maximum (map treedepth t)
