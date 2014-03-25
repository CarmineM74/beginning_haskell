{-# LANGUAGE LambdaCase #-}
module Chapter4.Containers where

import Chapter4.Model
import qualified Data.Map as M
import qualified Data.Set as S

myinsert :: Ord k => k -> a -> M.Map k a -> M.Map k a
myinsert k v m = M.alter (\_ -> Just v) k m

mydelete :: Ord k => k -> M.Map k a -> M.Map k a
mydelete k m = M.alter (\_ -> Nothing) k m

myadjust :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
myadjust f k m = M.alter (\case Nothing -> Nothing
                                (Just v) -> Just (f v)
                         ) k m

-- Ex 4.3
classifyClients :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients = undefined

