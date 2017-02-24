-----------------------------------------------------------------------------
-- |
-- Module      :  Trees.Tree
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- A type for trees
-- with
-- values in leaves,
-- values in nodes, and
-- indexed subtrees.
-----------------------------------------------------------------------------

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

import           Control.Arrow    as A
import           Data.Map         as M (fromList)
import           Trees.Trifunctor

-- |A type for trees with
-- values of type a in leaves and
-- values of type b in nodes.
-- The subtrees of a node are indexed by indexes of type c.
data Tree a b c
  -- |a 'Leaf' in a 'Tree'.
  -- It holds a value of type a.
  = Leaf a
  -- |a 'Node' in a 'Tree'.
  -- It holds a value of type b and a list of subtrees indexed type c.
  -- It is possible to have more than one subtree for a given index.
  -- Since we want 'Tree's to be stable upon application of a 'Trifunctor',
  -- we use a list of couples instead of a map.
  | Node b [(c, Tree a b c)]
  deriving (Show)

-- |Eq for a 'Tree',
-- two 'Tree's are == independently of the ordering of children.
--
-- Note: the instance has to be explicited here since we do not use a map.
instance (Eq a, Eq b, Ord c) => Eq (Tree a b c) where
  (Leaf x) == (Leaf y) = x == y
  (Node x ts1) == (Node y ts2) = (x == y) && (M.fromList ts1 == M.fromList ts2)
  _ == _ = False

-- |Trifunctor for a 'Tree',
-- applies to leaf values, node values, and node subtree indexes.
instance Trifunctor Tree where
  trimap f g h (Leaf x) = Leaf (f x)
  trimap f g h (Node x ts) =
    Node (g x)
          (fmap (h A.*** trimap f g h) ts)

-- |Check the validity of a 'Tree'.
--
-- A 'Leaf' is always valid. A 'Node' is valid iff:
--
-- - it has a least one subtree
isValidTree :: Tree a b c ->Bool
isValidTree (Leaf _)       =  True
isValidTree (Node _ (_:_)) = True
isValidTree (Node _ _)     = False

-- |Get the direct subtrees of a 'Tree' whose index satify a predicate.
--
-- If no direct subtree index satisfy the predicate
-- or if the 'Tree' is a 'Leaf',
-- then return an empty list.

directSubtreesSuchThat :: (c -> Bool) -> Tree a b c -> [Tree a b c]
directSubtreesSuchThat _ (Leaf _)    = []
directSubtreesSuchThat p (Node _ ts) = [t | (n,t) <- ts,p n]

-- |Get the direct subtrees of a 'Tree'.
--
-- If the 'Tree' is a 'Leaf' then return an empty list.
directSubtrees :: Tree a b c -> [Tree a b c]
directSubtrees = directSubtreesSuchThat (const True)

-- |Get the direct subtrees of a 'Tree' for a given key.
--
-- If the key does not exist,
-- or if the 'Tree' is a 'Leaf'
-- then return an empty list.
directSubtreesFor :: Eq c => c -> Tree a b c -> [Tree a b c]
directSubtreesFor i = directSubtreesSuchThat (== i)

-- |Get the list of all the values in leaves in the 'Tree'.
--
-- The list is obtained using a DFS traversal of the 'Tree'.
leafValues :: Tree a b c -> [a]
leafValues (Leaf x)      = [x]
leafValues t@(Node _ ts) = concat $ directSubtreeMap leafValues t

-- |Get the list of all the values in nodes in a 'Tree'.
--
-- The list is obtained using a DFS traversal of the 'Tree'.
nodeValues :: Tree a b c -> [b]
nodeValues (Leaf _)      = []
nodeValues t@(Node x ts) = x : concat (directSubtreeMap nodeValues t)

-- |Get the depth of a 'Tree'.
depth :: (Ord t, Num t) => Tree a b c -> t
depth (Leaf _)     = 1
depth t@(Node _ _) = 1 + maximum (directSubtreeMap depth t)

-- |Helper to apply a function to all direct subtrees of a 'Node'.
directSubtreeMap :: (Tree a b c -> d) -> Tree a b c -> [d]
directSubtreeMap f t = fmap f (directSubtrees t)
