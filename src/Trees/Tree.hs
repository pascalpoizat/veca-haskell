{-|
Module      : Trees.Tree
Description : A type for trees with values in leaves and in nodes, with indexed subtrees.
Copyright   : (c) 2017 Pascal Poizat
License     : Apache-2.0 (see the file LICENSE)
Maintainer  : pascal.poizat@lip6.fr
Stability   : experimental
Portability : unknown
-}
module Trees.Tree (
    -- * constructors
    Tree(..)
    -- * validity checking
  , isValidTree
    -- * value retrieval
  , directSubtreesSuchThat
  , directSubtrees
  , directSubtreesFor
  , leafValues
  , nodeValues
    -- * properties
  , depth)
where

import           Control.Arrow
import           Data.Map         (fromList)
import           Trees.Trifunctor

{-|
A type for trees with values of type a in leaves and values of type b in nodes.
The subtrees of a node are indexed by indexes of type c.

It is possible to have more than one subtree for a given index.
Since we want trees to be stable upon application of a trifunctor,
we use a list of couples instead of a map.
-}
data Tree a b c
  = Leaf a
  | Node b
         [(c, Tree a b c)]
  deriving (Show)

{-|
Eq instance for trees.

Two trees are equals independently of the ordering of children.
-}
instance (Eq a, Eq b, Ord c) => Eq (Tree a b c) where
  (Leaf x) == (Leaf y) = x == y
  (Node x ts1) == (Node y ts2) = (x == y) && (fromList ts1 == fromList ts2)
  _ == _ = False

{-|
Ord instance for trees.

t1 compare t2 yields EQ iff t1 == t2.
-}
instance (Eq a, Eq b, Ord a, Ord b, Ord c) => Ord (Tree a b c) where
  (Leaf x) `compare` (Leaf y) = x `compare` y
  (Leaf _) `compare` (Node _ _) = LT
  (Node _ _) `compare` (Leaf _) = GT
  (Node x ts) `compare` (Node x' ts')
    | x == x' = fromList ts `compare` fromList ts'
    | otherwise = x `compare` x'

{-|
Trifunctor instance for trees.

Applies to leaf values, node values, and node subtree indexes.
-}
instance Trifunctor Tree where
  trimap f _ _ (Leaf x)    = Leaf (f x)
  trimap f g h (Node x ts) = Node (g x) ((h *** trimap f g h) <$> ts)

{-|
Check the validity of a tree.

A leaf is always valid.
A node is valid iff it has a least one subtree.
-}
isValidTree :: Tree a b c -> Bool
isValidTree (Node _ []) = False
isValidTree _           = True

{-|
Get the direct subtrees of a tree whose index satify a predicate.

If no direct subtree index satisfy the predicate or if the tree is a leaf,
then return an empty list.
-}
directSubtreesSuchThat :: (c -> Bool) -> Tree a b c -> [Tree a b c]
directSubtreesSuchThat _ (Leaf _)    = []
directSubtreesSuchThat p (Node _ ts) = [t | (n, t) <- ts, p n]

{-|
Get the direct subtrees of a tree.

If the tree is a leaf then return an empty list.
-}
directSubtrees :: Tree a b c -> [Tree a b c]
directSubtrees = directSubtreesSuchThat (const True)

{-|
Get the direct subtrees of a tree for a given key.

If the key does not exist or if the tree is a leaf,
then return an empty list.
-}
directSubtreesFor :: Eq c => c -> Tree a b c -> [Tree a b c]
directSubtreesFor i = directSubtreesSuchThat (== i)

{-|
Get the list of all the values in leaves in the tree.

The list is obtained using a DFS traversal.
-}
leafValues :: Tree a b c -> [a]
leafValues (Leaf x)      = [x]
leafValues t@(Node _ _) = concat $ directSubtreeMap leafValues t

{-|
Get the list of all the values in nodes in a tree.

The list is obtained using a DFS traversal.
-}
nodeValues :: Tree a b c -> [b]
nodeValues (Leaf _)      = []
nodeValues t@(Node x _) = x : concat (directSubtreeMap nodeValues t)

{-|
Get the depth of a tree.
-}
depth :: (Ord d, Num d) => Tree a b c -> d
depth (Leaf _)     = 1
depth t@(Node _ _) = 1 + maximum (directSubtreeMap depth t)

{-|
Helper to apply a function to all direct subtrees of a node.
-}
directSubtreeMap :: (Tree a b c -> d) -> Tree a b c -> [d]
directSubtreeMap f t = f <$> directSubtrees t
