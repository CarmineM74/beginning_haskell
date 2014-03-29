{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Chapter4.Model(Person(..),
                      Client(..),
                      ClientKind(..),
                      classifyClient,
                      clients,
                      TravelGuide(..),
                      Tools(..),
                      guides,
                      BinaryTree2(..),
                      treeInsert2,
                      mytree1,
                      mytree2,
                      mytree3
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
-- *** skipped ***


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
                    deriving (Show, Ord, Eq)

data Tools = FluxCapacitor { tPrice :: Double }
            | AntimatterEncapsulator { tPrice :: Double }
            | NutsAndBolts { tPrice :: Double }
            | MagLevInjector { tPrice :: Double }
            deriving (Show,Ord,Eq)

guides :: [TravelGuide]
guides = [TravelGuide "Hitchickers' guide to the galaxy" ["various"] 12.3, TravelGuide "Universe's boundaries" ["Carmine Moleti"] 8.32, TravelGuide "Through the wormhole" ["Morgan Freeman"] 9.1]

data BinaryTree = Node TravelGuide BinaryTree BinaryTree
                | Leaf
                deriving Show

treeFind :: TravelGuide -> BinaryTree -> Maybe TravelGuide
treeFind t (Node v l r) = case compare t v of
                            EQ -> Just v
                            LT -> treeFind t l
                            GT -> treeFind t r
treeFind _ Leaf = Nothing

treeInsert :: TravelGuide -> BinaryTree -> BinaryTree
treeInsert t n@(Node v l r) = case compare t v of
                                EQ -> n
                                LT -> Node v (treeInsert t l) r
                                GT -> Node v l (treeInsert t r)
treeInsert t Leaf = Node t Leaf Leaf

data BinaryTree2 a = Node2 a ( BinaryTree2 a ) ( BinaryTree2 a )
                    | Leaf2
                    deriving Show
                    
treeFind2 :: Ord a => a -> BinaryTree2 a -> Maybe a
treeFind2 t (Node2 v l r) = case compare t v of
                              EQ -> Just v
                              LT -> treeFind2 t l
                              GT -> treeFind2 t r
treeFind2 _ Leaf2 = Nothing

-- Ex 4.7
-- Make the changes needed in treeInsert to work with the new BinaryTree2.
-- Also, try to implement concatenation of binary trees by repeated insertion of all the elements in
-- one of the binary trees.

treeInsert2 :: Ord a => a -> BinaryTree2 a -> BinaryTree2 a
treeInsert2 t n@(Node2 v l r) = case compare t v of
                                EQ -> n
                                LT -> Node2 v (treeInsert2 t l) r
                                GT -> Node2 v l (treeInsert2 t r)
treeInsert2 t Leaf2 = Node2 t Leaf2 Leaf2

concatTree :: Ord a => BinaryTree2 a -> BinaryTree2 a -> BinaryTree2 a
concatTree t1@(Node2 v l r) t2 = concatTree r (concatTree l (treeInsert2 v t2))
concatTree Leaf2 t2 = t2

mytree1 :: BinaryTree2 Integer
mytree1 = Node2 8 (Node2 6 Leaf2 (Node2 2 Leaf2 Leaf2)) (Node2 9 Leaf2 (Node2 10 Leaf2 Leaf2))

mytree2 :: BinaryTree2 Integer
mytree2 = Node2 33 (Node2 7 (Node2 5 Leaf2 Leaf2) (Node2 8 Leaf2 Leaf2)) (Node2 61 (Node2 22 Leaf2 Leaf2) (Node2 80 Leaf2 Leaf2)) 

mytree3 :: BinaryTree2 Integer
mytree3 = Node2 1 Leaf2 (Node2 2 Leaf2 (Node2 3 Leaf2 (Node2 4 Leaf2 Leaf2)))
