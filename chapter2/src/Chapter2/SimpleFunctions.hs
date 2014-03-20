{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Chapter2.SimpleFunctions where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

(+++) :: [a] -> [a] -> [a]
lst1 +++ lst2 = if null lst1
                  then lst2
                  else (head lst1) : (tail lst1 +++ lst2)

reverse2 :: [a] -> [a]
reverse2 lst = if null lst
                then []
                else reverse (tail lst) +++ [head lst]

data Person = Person String String Gender deriving Show
data Gender = Male | Female | Unknown deriving Show
data Client = GovOrg String
            | Company String Integer Person String
            | Individual Person Bool
            deriving Show

data TimeMachine = TimeMachine String Integer String TravelCapabilities Float deriving Show
data TravelCapabilities = Past | Future | Both deriving Show

data GenderStats = GenderStats (Integer,Integer,Integer) deriving Show

countClientGenders :: [Client] -> GenderStats
countClientGenders [] = GenderStats (0,0,0)
countClientGenders ((Individual (Person _ _ gender) _):xs) = 
        case gender of
          Male ->     GenderStats (1+total_males,total_females,total_unkowns)
          Female ->   GenderStats (total_males, 1+total_females, total_unkowns)
          Unknown ->  GenderStats (total_males, total_females, 1+total_unkowns)

        where
          GenderStats (total_males,total_females,total_unkowns) = countClientGenders xs
countClientGenders (_:xs) = countClientGenders xs

clients :: [Client]
clients = [GovOrg "NASA", Company "Google" 1 (Person "Erich" "Smidth" Male) "CEO", GovOrg "NSA", Individual (Person "Carmine" "Moleti" Male) False, Company "FrigoCaserta srl" 2 (Person "Paolo" "Salzillo" Male) "CEO", Individual (Person "Anna" "Del Prete" Female) True, Individual (Person "Mr." "X" Unknown) False, Individual (Person "Enrico" "Moleti" Male) False]

applyDiscount :: [TimeMachine] -> [TimeMachine]
applyDiscount [] = []
applyDiscount ((TimeMachine manuf model name capabilities price):ms) = (TimeMachine manuf model name capabilities discounted_price) : (applyDiscount ms)
  where
    discounted_price = price - (price * 5.0 / 100.0)

machines :: [TimeMachine]
machines = [TimeMachine "ACME Inc" 1 "A glimpse to the past" Past 1000.0, TimeMachine "BTF" 38 "Flux capacitor" Both 3865.0]

myempty :: [a] -> Bool
myempty [] = True
myempty (_:_) = False

myhead :: [a] -> a
myhead (x:_) = x

mytail :: [a] -> [a]
mytail [] = []
mytail (_:xs) = xs

ackermann :: Integer -> Integer -> Integer
ackermann m n 
  | (m == 0) = n + 1
  | (m>0) && (n == 0) = ackermann (m-1) 1
  | otherwise = ackermann (m-1) (ackermann m (n-1))

myunzip :: [(a,a)] -> ([a],[a])
myunzip [] = ([],[])
myunzip [(x,y)] = ([x],[y])
myunzip ((x,y):(k,z):cs) = (x:k:lefts,y:z:rights)
    where
      (lefts,rights) = myunzip cs


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

greet :: ClientR -> String
greet CompanyR { clientRName } = "Hello, " ++ clientRName
greet GovOrgR {} = "Welcome"
greet IndividualR { person = PersonR { firstName } } = "Hi, " ++ firstName

erich :: PersonR
erich = PersonR { firstName = "Erich", lastName = "Schmidt" }

individual :: ClientR
individual = IndividualR {person = erich}

company :: ClientR
company = CompanyR { clientRName = "Google Inc.", companyId = 1, person = erich, duty = "CEO" }
