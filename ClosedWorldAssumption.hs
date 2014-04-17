module ClosedWorldAssumption where

--class MyEq a where
--  myeq :: a -> a -> Bool

--instance Eq a => MyEq a where
--  myeq = (==)

data MyEq2 a = MyEq2 a

instance Eq a => Eq (MyEq2 a) where
  (MyEq2 x) == (MyEq2 y) = x == y
