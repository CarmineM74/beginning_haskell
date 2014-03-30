module Chapter5.Laziness where

data TimeMachine = TM {manufacturer :: String, year :: Integer}
                    deriving (Eq, Show)

timeMachinesFrom :: String -> Integer -> [TimeMachine]
timeMachinesFrom mf y = TM mf y : timeMachinesFrom mf (y+1)

timelyIncMachines :: [TimeMachine]
timelyIncMachines = timeMachinesFrom "Timely Inc." 100

-- Ex 5.1
-- Sieve of Eratosthenes
-- Start from all the numbers from 2 ..
-- Take the first number and drop from the list all its multiples (remainder (n,2) is 0)
-- Take the next number in the filtered list and repeat the operation filtering out all its multiples
-- Repeat the last step
-- iterate f x = [x, f x, f (f x), .. ]
primes :: [Integer]
primes =  map fst $ iterate f (2,[3..])
  where
    f (h,rest) = (head rest, filter (\x -> rem x h /= 0) (tail rest))
