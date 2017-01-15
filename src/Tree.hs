-- module for Trees
-- (c) 2016 Pascal Poizat
-- github: @pascalpoizat
--
module Tree ( Tree(..)
            , trimap
            , isValid
            , leafValues
            , nodeValues
            , directSubtreesSuchThat
            , directSubtrees
            , directSubtreesFor
            , depth)
where

import           Trifunctor

-- Tree with leaf values of type a, node values of type b, and
-- node subtree indexes of type c.
-- We use couples (c, Tree a b c) for the subtrees of a node
-- in order to have a trifunctor (the structure is kept, not with Map c (Tree a b c))
data Tree a b c
  = Leaf a
  | Node b [(c, Tree a b c)]
  deriving (Show,Eq)

-- Trifunctor that transforms leaf values, node values, and node subtree names
-- trimap :: (a -> a') -> (b -> b') -> (c -> c') -> Tree a b c -> Tree a' b' c'
instance Trifunctor Tree where
  trimap f g h (Leaf x) = Leaf (f x)
  trimap f g h (Node x ts) =
    Node (g x)
         (map (\(c,t) -> (h c, trimap f g h t)) ts)

-- checks for the validity of a tree
isValid :: Tree a b c -> Bool
isValid (Leaf _)    = True -- leaves are valid
isValid (Node _ ts) = not (null ts) -- nodes are valid if they have at least a subtree

-- get the depth of the tree
depth :: (Ord t, Num t) => Tree a b c -> t
depth (Leaf _)     = 1
depth t@(Node _ _) = 1 + (maximum $ directSubtreemap depth t)

-- extract leaf values (DFS)
leafValues :: Tree a b c -> [a]
leafValues (Leaf x)      = [x]
leafValues t@(Node _ ts) = concat $ directSubtreemap leafValues t

-- extract node values (DFS)
nodeValues :: Tree a b c -> [b]
nodeValues (Leaf _)      = []
nodeValues t@(Node x ts) = x : (concat $ directSubtreemap nodeValues t)

-- get the direct subtrees of a tree given a predicate on index
directSubtreesSuchThat :: (c -> Bool) -> Tree a b c -> [Tree a b c]
directSubtreesSuchThat _ (Leaf _)    = []
directSubtreesSuchThat p (Node _ ts) = [t|(n,t) <- ts,p n]

-- get the direct subtrees of a tree
directSubtrees :: Tree a b c -> [Tree a b c]
directSubtrees = directSubtreesSuchThat (\x -> True)

-- get the direct subtrees of a tree for a given index
directSubtreesFor :: Eq c => c -> Tree a b c -> [Tree a b c]
directSubtreesFor i = directSubtreesSuchThat (== i)

-- helper (TODO refactor using Lens.Plated later on)
directSubtreemap :: (Tree a b c -> d) -> Tree a b c -> [d]
directSubtreemap f t = map f $ directSubtrees t
