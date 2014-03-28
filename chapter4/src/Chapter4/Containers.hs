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
-- The first should traverse the list element by element, and perform on each element
-- the classification, decide which of the map items to modify, and then add itself to
-- the set;
classifyClients1 :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients1 =  foldr insertClient newMap
  where
    newMap = M.fromList $ zip [GovOrgKind,CompanyKind,IndividualKind] [S.empty,S.empty,S.empty]
    insertClient c m = M.adjust (S.insert c) clientClass m
      where
        clientClass = classifyClient c

-- The second should first create lists corresponding to the three different kinds, and at
-- the end convert those lists to sets and generate the mentioned map from them.
classifyClients2 :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients2 clients = M.fromList $ zip [GovOrgKind,CompanyKind,IndividualKind] $ map S.fromList [govs,companies,individuals]
  where
    (govs,companies,individuals) = classification clients

classification :: [Client Integer] -> ([Client Integer], [Client Integer], [Client Integer])
classification = foldr f ([],[],[])
  where
    f c (gs,cs,is) 
      | clientClass == GovOrgKind = (c:gs,cs,is)
      | clientClass == CompanyKind = (gs,c:cs,is)
      | clientClass == IndividualKind = (gs,cs,c:is)
      where
        clientClass = classifyClient c
