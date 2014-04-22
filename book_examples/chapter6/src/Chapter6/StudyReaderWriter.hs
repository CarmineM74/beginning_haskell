module Chapter6.StudyReaderWriter where

import Data.Monoid

newtype MyWriter m a = MyWriter (a,m)

instance Monoid m => Monad (MyWriter m) where
  -- return :: a -> MyWriter m a
  return x = MyWriter (x,mempty)
  -- >>= :: (MyWriter m a) -> (a -> MyWriter m b) -> (Mywriter m b)
  (>>=) (MyWriter (x,s)) f = MyWriter (y,mappend s s')
                            where
                              MyWriter (y,s') = f x 

myTell :: Monoid m => m -> MyWriter m () 
myTell x = MyWriter ((),x)
