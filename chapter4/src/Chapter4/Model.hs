{-# LANGUAGE RecordWildCards #-}

module Chapter4.Model(Person(..),
                      Client(..),
                      ClientKind(..),
                      classifyClient,
                      clients
                     ) where

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                         , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Ord, Eq)

data Person = Person { firstName :: String, lastName  :: String }
              deriving (Show,Ord,Eq)

data ClientKind = GovOrgKind | CompanyKind | IndividualKind
              deriving (Show,Ord,Eq)

classifyClient :: Client a -> ClientKind
classifyClient (GovOrg {..}) = GovOrgKind
classifyClient (Company {..}) = CompanyKind
classifyClient (Individual {..}) = IndividualKind

clients :: [Client Integer]
clients = [GovOrg 1 "NASA", 
          Company 2 "Google" (Person "Erich" "Smidth") "CEO", 
          GovOrg 3 "NSA", 
          Individual 4 (Person "Carmine" "Moleti"), 
          Company 5 "FrigoCaserta srl" (Person "Paolo" "Salzillo") "CEO", 
          Company 6 "FrigoCaserta srl" (Person "Gennaro" "Gianoglio") "WareHouse", 
          Company 7 "FrigoCaserta srl" (Person "Carmine" "Moleti") "CED", 
          Individual 8 (Person "Anna" "Del Prete"), 
          Individual 9 (Person "Mr." "X"), 
          Individual 10 (Person "Enrico" "Moleti")]

