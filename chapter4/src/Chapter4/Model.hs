{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Chapter4.Model(Person(..),
                      Client(..),
                      ClientKind(..),
                      classifyClient,
                      clients,
                      TravelGuide(..),
                      Tools(..),
                      guides
                     ) where

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                         , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Ord)

data Person = Person { firstName :: String, lastName  :: String }
              deriving (Show,Ord)

-- Ex 4.5
-- Implement Eq for the Person and Client i types introduced in the previous chapters, so
-- that it checks the equality of all the fields. Notice that for Client you may need to add some
-- restrictions over the type of the identifiers.

instance Eq Person where
  --(Person {firstName = fname1, lastName = lname1}) == (Person {firstName = fname2, lastName = lname2}) = (fname1 == fname2) && (lname1 == lname2)
  (Person fname1 lname1) == (Person fname2 lname2) = (fname1 == fname2) && (lname1 == lname2)

instance Eq i => Eq (Client i) where
  (==) (GovOrg id1 name1) (GovOrg id2 name2) = (id1 == id2) && (name1 == name2)
  (==) (Company id1 name1 person1 duty1) (Company id2 name2 person2 duty2) = (id1 == id2) && (name1 == name2) && (person1 == person2) && (duty1 == duty2)
  (==) (Individual id1 person1) (Individual id2 person2) = (id1 == id2) && (person1 == person2)

-- Ex 4.6
-- The automatically derived Ord instance for Clients doesn't make much sense: as discussed
-- above, it checks the kind of client and then its identifier. Instead of that, write a new instance
-- with the following properties: first it should compare the name of the client. Then, if they
-- coincide, it should put individuals first, then companies and government organizations at the end.
-- You may need to add further comparisons in the rest of the fields to make the order correct (for
-- example, two companies whose responsibles are different are not equal, so you must decide
-- which one to put first).
-- Think beforehand whether you need to include some restriction in the instance.



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

data TravelGuide = TravelGuide { title :: String, authors :: [String], tgPrice :: Double}
                    deriving (Show)

data Tools = FluxCapacitor { tPrice :: Double }
            | AntimatterEncapsulator { tPrice :: Double }
            | NutsAndBolts { tPrice :: Double }
            | MagLevInjector { tPrice :: Double }
            deriving (Show)

guides :: [TravelGuide]
guides = [TravelGuide "Hitchickers' guide to the galaxy" ["various"] 12.3, TravelGuide "Universe's boundaries" ["Carmine Moleti"] 8.32, TravelGuide "Through the wormhole" ["Morgan Freeman"] 9.1]
