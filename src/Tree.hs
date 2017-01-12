-- module for Trees
-- (c) 2016 Pascal Poizat
-- github: @pascalpoizat
--
module Tree ( Tree(..)
            , trimap
            , isValid
            , leafValues
            , nodeValues
            , subtreesSuchThat
            , subtrees
            , subtreesFor
            , depth)
where

import           Trifunctor

-- Tree with leaf values of type a, node values of type b, and
-- node subtree names of type c.
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
isValid (Leaf _)    = True
isValid (Node _ ts) = not (null ts)

-- get the subtrees given a predicate on index
subtreesSuchThat :: (c -> Bool) -> Tree a b c -> [Tree a b c]
subtreesSuchThat _ (Leaf _)    = []
subtreesSuchThat p (Node _ ts) = [t|(n,t) <- ts,p n]

-- get the subtrees of a tree
subtrees :: Tree a b c -> [Tree a b c]
subtrees = subtreesSuchThat (\x -> True)

-- get the subtrees for a given index
subtreesFor :: Eq c => c -> Tree a b c -> [Tree a b c]
subtreesFor i = subtreesSuchThat (\x -> x == i)

-- get the depth of the tree
depth :: (Ord t, Num t) => Tree a b c -> t
depth (Leaf _)     = 1
depth t@(Node _ _) = 1 + (maximum $ subtreemap depth t)

-- extract leaf values (DFS)
leafValues :: Tree a b c -> [a]
leafValues (Leaf x)      = [x]
leafValues t@(Node _ ts) = concat $ subtreemap leafValues t

-- extract node values (DFS)
nodeValues :: Tree a b c -> [b]
nodeValues (Leaf _)      = []
nodeValues t@(Node x ts) = x : (concat $ subtreemap nodeValues t)

-- helper (TODO refactor using Lens.Plated later on)
subtreemap :: (Tree a b c -> d) -> Tree a b c -> [d]
subtreemap f t = map f $ subtrees t
