module Chapter7.MonadFun where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Set as S

myFilterM :: MonadPlus m => (a -> m Bool) -> [a] -> m [a]
myFilterM _ [] = return []
myFilterM p (x:xs) = do
                      res <- p x
                      res' <- myFilterM p xs
                      return $ if res then x:res' else res'

brokenJump :: Int -> [Int]
brokenJump y = [y-1,y+3,y+5]

brokenThreeJumps :: Int -> [Int]
brokenThreeJumps year = do
                          first <- brokenJump year
                          second <- brokenJump first
                          brokenJump second

brokenJumps :: Int -> Int -> [Int]
brokenJumps 0 year = [year]
brokenJumps n year = do
                      jumpTo <- brokenJump year
                      brokenJumps (n-1) jumpTo

brokenJumps' :: Int -> Int -> [Int]
brokenJumps' n year = brokenJumps'' n [year] 
                  where
                    brokenJumps'' 0 years = years
                    brokenJumps'' i years = years >>= brokenJumps'' (i-1) . brokenJump

-- ex 7.2
find' :: (a -> Bool) -> [a] -> Maybe a
find' p xs = msum $ map (\x -> if p x then Just x else Nothing) xs


generateL1 :: Int -> [Int] -> [S.Set Int]
generateL1 t xs = do
                    x <- xs
                    guard (x > t)
                    return $ S.singleton x

addPrefix :: String -> Reader String String
addPrefix s = ask >>= \p -> return $ p ++ s

-- 7.5
sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (x:xs) = do
                v <- x
                vs <- sequence' xs
                return (v:vs)

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f = sequence' . map f

factorialSteps :: Integer -> Writer (Sum Integer) Integer
factorialSteps n = foldM f 1 [1..n]
                where
                  f a x = tell (Sum 1) >> return (a*x)

testW :: [String] -> WriterT String [] ()
testW xs = do
            x <- lift xs
            tell x

statePut :: Integer -> State Integer ()
statePut x = put (x+1)

stateModify :: Integer -> State Integer ()
stateModify x = modify (+x)

dLog :: String -> WriterT [String] Maybe Int
dLog st = tell [st] >> return 1974

doubler :: ReaderT [Int] [] Int
doubler = do 
            values <- ask
            value <- lift values
            return $ value * 2

wt :: Int -> WriterT [Int] [] ()
wt s = do
        tell $ [s*2]

-- 7.6
factorialT' :: StateT Integer (State Integer) Integer
factorialT' = do
                cnt <- get
                cur_fact <- lift get
                if cnt == 0 
                  then return cur_fact
                  else do
                          lift (put (cur_fact*cnt)) -- (put ...) must be within parentheses. See bugs 7368 and 7920
                          modify (\x -> x-1)
                          factorialT'

factorialT :: Integer -> Integer
factorialT n = execState (execStateT factorialT' n) 1

-- 7.7
graph1 :: [(Int, Int)]
graph1 = [(2013,501),(2013,1004),(501,2558),(1004,2558)]

mypaths :: [(Int,Int)] -> Int -> Int -> [[Int]]
mypaths edges start end = execWriterT (runReaderT (mypathsT start end) edges)

mypathsT :: Int -> Int -> ReaderT [(Int,Int)] (WriterT [Int] []) ()
mypathsT start end = if start == end 
                      then lift (tell [start]) `mplus` e_paths
                      else e_paths
                    where
                      e_paths = do
                                  edges <- ask
                                  (e_start, e_end) <- lift . lift $ edges
                                  guard $ e_start == start
                                  lift (tell [start])
                                  mypathsT e_end end

