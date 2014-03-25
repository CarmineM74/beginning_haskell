{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Chapter3.Model(PersonR(..),
                      ClientR(..),
                      clientName',
                      isCompany,
                      clients) where

data PersonR = PersonR { firstName :: String
                    , lastName :: String
                    } deriving Show

data ClientR = GovOrgR { clientRName :: String}
          | CompanyR { clientRName :: String
                    , companyId :: Integer
                    , person :: PersonR
                    , duty :: String
                    }
          | IndividualR { person :: PersonR }
          deriving Show

clientName' :: ClientR -> String
clientName' GovOrgR { .. } = clientRName
clientName' CompanyR { .. } = clientRName
clientName' IndividualR { person = PersonR { ..} } = firstName ++ " " ++ lastName

clients :: [ClientR]
clients = [GovOrgR "NASA", 
          CompanyR "Google" 1 (PersonR "Erich" "Smidth") "CEO", 
          GovOrgR "NSA", IndividualR (PersonR "Carmine" "Moleti"), 
          CompanyR "FrigoCaserta srl" 2 (PersonR "Paolo" "Salzillo") "CEO", 
          CompanyR "FrigoCaserta srl" 2 (PersonR "Gennaro" "Gianoglio") "WareHouse", 
          CompanyR "FrigoCaserta srl" 2 (PersonR "Carmine" "Moleti") "CED", 
          IndividualR (PersonR "Anna" "Del Prete"), 
          IndividualR (PersonR "Mr." "X"), 
          IndividualR (PersonR "Enrico" "Moleti")]

isCompany :: ClientR -> Bool
isCompany (CompanyR {}) = True
isCompany _ = False

data ClientKind = GovOrgKind | CompanyKind | IndividualKind
