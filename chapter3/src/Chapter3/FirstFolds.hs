{-# LANGUAGE NamedFieldPuns #-}

module Chapter3.FirstFolds where

import Chapter3.Model

myproduct :: [Integer] -> Integer
myproduct [] = 1
myproduct (x:xs) = x * myproduct xs

myproductFold :: [Integer] -> Integer
myproductFold = foldr (*) 1

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


