-- module for Trees
-- (c) 2016 Pascal Poizat
-- github: @pascalpoizat
--
module Tree ( Tree(..)
            , trimap
            , isValid
            , children
            , childrenFor
            , leaves
            , depth)
where

import           Data.Set   (Set, singleton, unions)
import           Trifunctor

-- tree with values of type a in the leaves and values of type b in the nodes
-- nodes link to children given a key of type c
-- we use a list of couples (c, Tree a b c) to enable us to use trifunctors
-- (the structure is kept, which is not the case of (Map c (Tree a b c))
data Tree a b c
  = Leaf a
  | Node b [(c, Tree a b c)]
  deriving (Show,Eq)

-- apply transformation f to the leaf information,
-- g to the node information, and
-- h to the indexes
-- trimap :: (a -> a') -> (b -> b') -> (c -> c') -> Tree a b c -> Tree a' b' c'
instance Trifunctor Tree where
  trimap f g h (Leaf x) = Leaf (f x)
  trimap f g h (Node x ts) =
    Node (g x)
         (map (\(c,t) -> (h c,(trimap f g h) t)) ts)

-- checks for the validity of a tree
isValid :: Tree a b c -> Bool
isValid (Leaf _)    = True
isValid (Node _ ts) = not (null ts)

-- get the subtrees of a tree
children :: Tree a b c -> [Tree a b c]
children (Leaf _)    = []
children (Node _ ts) = [t | (_, t) <- ts]

-- get the subtrees for a given index
childrenFor :: Eq c => c -> Tree a b c -> [Tree a b c]
childrenFor _ (Leaf _)    = []
childrenFor c (Node _ ts) = [t | (n, t) <- ts, n == c]

-- get the set of all leaves
leaves :: Ord a => Tree a b c -> Set a
leaves (Leaf x)     = singleton x
leaves t@(Node _ _) = unions (map leaves (children t))

-- get the depth of the tree
depth :: (Ord t, Num t) => Tree a b c -> t
depth (Leaf x)     = 1
depth t@(Node _ _) = 1 + maximum (map depth (children t))
