{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE RecordWildCards #-}

module Chapter3.ListAndPredicates where

import Chapter3.Model
import Data.List
import GHC.Exts

skipUntilGov :: [ClientR] -> [ClientR]
skipUntilGov = dropWhile (\case { GovOrgR {} -> False; _ -> True})

isIndividual :: ClientR -> Bool
isIndividual (IndividualR {}) = True
isIndividual _                = False

checkIndividualAnalytics :: [ClientR] -> (Bool, Bool)
checkIndividualAnalytics cs = (any isIndividual cs, not $ all isIndividual cs)

compareClient :: ClientR -> ClientR -> Ordering
compareClient (IndividualR{person = p1}) (IndividualR{person = p2}) = compare (firstName p1) (firstName p2)
compareClient _ (IndividualR{}) = LT
compareClient (IndividualR{}) _ = GT
compareClient c1 c2 = compare (clientName' c1) (clientName' c2)

companyDutiesAnalytics :: [ClientR] -> [String]
companyDutiesAnalytics = map (duty . head) . sortBy (\x y -> compare (length y) (length x)) .groupBy (\x y -> duty x == duty y) . filter isCompany

companyAnalytics :: [ClientR] -> [(String,[(PersonR, String)])]
companyAnalytics cs = [ (the clientRName,zip person duty) | c@(CompanyR { .. }) <- cs, 
                        then sortWith by duty,
                        then group by clientRName using groupWith, -- from this point on, all previous bindings are considered to be lists. 
                                                                   -- Thus clientRName, person and duty are to be considered as lists.
                        then sortWith by length c]
