module Chapter6.StudyState where

import Control.Monad.State

fun :: State String Integer -> Integer -> State String Integer
fun os x = state $ \s -> let s' = execState os s in (x,s'++"!") 

fun' :: Integer -> State String Integer
fun' x = do
          s <- get
          put $ s ++ "!"
          return x
    
