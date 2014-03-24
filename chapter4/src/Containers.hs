{-# LANGUAGE LambdaCase #-}
module Chapter4.Containers where

import qualified Data.Map as M

myinsert :: Ord k => k -> a -> M.Map k a -> M.Map k a
myinsert k v m = M.alter (\_ -> Just v) k m

mydelete :: Ord k => k -> M.Map k a -> M.Map k a
mydelete k m = M.alter (\_ -> Nothing) k m

myadjust :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
myadjust f k m = M.alter (\case Nothing -> Nothing
                                (Just v) -> Just (f v)
                         ) k m
