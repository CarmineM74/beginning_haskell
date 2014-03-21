{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Chapter3.FirstFolds where

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

myproduct :: [Integer] -> Integer
myproduct [] = 1
myproduct (x:xs) = x * myproduct xs

myproductFold :: [Integer] -> Integer
myproductFold = foldr (*) 1

clientName :: ClientR -> String
clientName GovOrgR { .. } = clientRName
clientName CompanyR { .. } = clientRName
clientName IndividualR { person = PersonR { ..} } = firstName ++ " " ++ lastName

clients :: [ClientR]
clients = [GovOrgR "NASA", CompanyR "Google" 1 (PersonR "Erich" "Smidth") "CEO", GovOrgR "NSA", IndividualR (PersonR "Carmine" "Moleti"), CompanyR "FrigoCaserta srl" 2 (PersonR "Paolo" "Salzillo") "CEO", IndividualR (PersonR "Anna" "Del Prete"), IndividualR (PersonR "Mr." "X"), IndividualR (PersonR "Enrico" "Moleti")]

minClientAcc :: String -> [ClientR] -> String
minClientAcc cur clis
            | null clis = cur
            | cur_len <= cli_len = minClientAcc cur (tail clis) 
            | otherwise = minClientAcc cli (tail clis)
            where
              cur_len = length cur
              cli = clientName $ head clis
              cli_len = length cli

minimumClient :: [ClientR] -> String
minimumClient [] = ""
minimumClient (c:cs) = minClientAcc (clientName c) cs

minimumClientFold :: [ClientR] -> String
minimumClientFold cs = foldr1 (\c a -> if (length c) <= (length a) then c else a) names
                where names = map clientName cs


