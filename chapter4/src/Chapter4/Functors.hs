module Chapter4.Functors where

import Chapter4.Model
import Data.Foldable

-- Ex 4.8
-- Functor is one of the most ubiquitous type classes in Haskell. For example, both Maybe and
-- the binary trees from last section are part of the family. The functor instance of Maybe is quite
-- interesting, because it allows to shorten code that just applies a function when the value is a
-- Just (a pattern matching plus a creation of the new value is just replace by a call to map).
-- Write the corresponding instances for both of them. You will need to create a newtype for
-- Maybe values in order to make the compiler happy. From all the binary tree types seen so far,
-- choose BinaryTree2 as the type for instantiating Functor. In this last case, remember that
-- you must respect the order invariant of the tree, so the best way to write the map function may
-- involve repeatedly calling treeInsert2 on an empty tree.

newtype MaybeFun a = MaybeFun (Maybe a) deriving Show

instance Functor MaybeFun where
  fmap f (MaybeFun (Just x)) = MaybeFun (Just (f x))
  fmap f (MaybeFun Nothing) = MaybeFun Nothing

flatten :: BinaryTree2 a -> [a]
flatten Leaf2 = []
flatten (Node2 v l r) = v:flatten l ++ flatten r

-- Problem here is that we cannot impose constraint Ord b on f
--instance Functor BinaryTree2 where
--  fmap f Leaf2 = Leaf2
--  fmap f (Node2 v l r) =  foldr g Leaf2 flattened
--    where
--      g v = treeInsert2 (f v)
--      flattened = v:flattened_left ++ flattened_right
--      flattened_left = flatten l
--      flattened_right = flatten r

instance Foldable MaybeFun where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr _ s (MaybeFun Nothing) = s
  foldr f s (MaybeFun (Just x)) = f x s

instance Foldable BinaryTree2 where
  foldr f s Leaf2 = s
  foldr f s (Node2 v l r) = Data.Foldable.foldr f (Data.Foldable.foldr f (f v s) l) r
