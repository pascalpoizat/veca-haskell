-----------------------------------------------------------------------------
-- |
-- Module      :  Tree
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache2.0 (see the file LICENSE)
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

module Tree ( Tree(..)
            -- * functor application
            , trimap
            -- * validity checking
            , isValid
            -- * value retrieval
            , leafValues
            , nodeValues
            -- * subtree retrieval
            , directSubtreesSuchThat
            , directSubtrees
            , directSubtreesFor
            -- * properties
            , depth)
where

import           Trifunctor

{- |
A type for trees with values of type a in leaves and values of type b in nodes.
The subtrees of a node are indexed by indexes of type c.
-}
data Tree a b c
  -- |a 'Leaf' in a 'Tree'.
  -- It holds a value of type a.
  = Leaf a
  -- |a 'Node' in a 'Tree'.
  -- It holds a value of type b and a list of subtrees indexed by indexes of type c.
  -- It is possible to have more than one subtree for a given index.
  -- Further, since we want the structure to be stable upon application of a trifunction,
  -- we use a list of couples instead of a map.
  | Node b [(c, Tree a b c)]
  deriving (Show,Eq)

{- |
One can apply three functions to a 'Tree':
one for the leaf values, one for the node values, and one for the node subtree indexes.
-}
instance Trifunctor Tree where
  trimap f g h (Leaf x) = Leaf (f x)
  trimap f g h (Node x ts) =
    Node (g x)
         (map (\(c,t) -> (h c, trimap f g h t)) ts)

{- |
The 'isValid' function checks if a 'Tree' is valid.
A tree is valid iff each of its nodes has at least one subtree.
-}
isValid :: Tree a b c -> Bool
isValid (Leaf _)    = True -- leaves are valid
isValid (Node _ ts) = not (null ts) -- nodes are valid if they have at least a subtree

{- |
The 'depth' function gets the depth of a 'Tree'.
-}
depth :: (Ord t, Num t) => Tree a b c -> t
depth (Leaf _)     = 1
depth t@(Node _ _) = 1 + (maximum $ directSubtreeMap depth t)

{- |
The 'leafValues' function gets the list of all the values in leaves in the 'Tree'.
The list is obtained using a DFS traversal of the 'Tree'.
-}
leafValues :: Tree a b c -> [a]
leafValues (Leaf x)      = [x]
leafValues t@(Node _ ts) = concat $ directSubtreeMap leafValues t

{- |
The 'nodeValues' function gets the list of all the values in nodes in a 'Tree'.
The list is obtained using a DFS traversal of the 'Tree'.
-}
nodeValues :: Tree a b c -> [b]
nodeValues (Leaf _)      = []
nodeValues t@(Node x ts) = x : (concat $ directSubtreeMap nodeValues t)

{- |
The 'directSubtreesSuchThat' function gets the direct subtrees of a 'Tree' whose index satify a predicate.
If no direct subtree index satisfy the predicate or if the 'Tree' is a 'Leaf' then it returns an empty list.
-}
directSubtreesSuchThat :: (c -> Bool) -> Tree a b c -> [Tree a b c]
directSubtreesSuchThat _ (Leaf _)    = []
directSubtreesSuchThat p (Node _ ts) = [t|(n,t) <- ts,p n]

{- |
The 'directSubtrees' function gets the direct subtrees of a 'Tree'.
If the 'Tree' is a 'Leaf' then it returns an empty list.
-}
directSubtrees :: Tree a b c -> [Tree a b c]
directSubtrees = directSubtreesSuchThat (\x -> True)

{- |
The 'directSubtreesFor' function gets the direct subtrees of a 'Tree' for a given key.
If the key does not exist, or if the 'Tree' is a 'Leaf' then it returns an empty list.
-}
directSubtreesFor :: Eq c => c -> Tree a b c -> [Tree a b c]
directSubtreesFor i = directSubtreesSuchThat (== i)

{- |
The directSubtreeMap function is a helper to apply a function to all direct subtrees of a 'Node'.
-}
directSubtreeMap :: (Tree a b c -> d) -> Tree a b c -> [d]
directSubtreeMap f t = map f $ directSubtrees t
