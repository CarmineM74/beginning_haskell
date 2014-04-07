{-# LANGUAGE TemplateHaskell #-}
module Chapter6.StudyLens where

import Control.Lens

data Client i = GovOrg { _identifier :: i, _name :: String }
            | Company { _identifier :: i, _name :: String, _person :: Person, _duty :: String }
            | Individual { _identifier :: i, _person :: Person }
            deriving Show

data Person = Person { _firstName :: String, _lastName :: String }
              deriving Show

makeLenses ''Client
makeLenses ''Person

data TimeMachine = TM {_manufacturer :: String, _year :: Integer, _price :: Double}
                    deriving (Eq, Show)

myMachines :: [TimeMachine]
myMachines = [
              TM "TimeTravellers Inc." 1900 1839.55,
              TM "Infinite and Beyond Co." 2450 6549.22,
              TM "Back to the future" 1982 553.66
            ]

makeLenses ''TimeMachine

increasePriceByPct :: [TimeMachine] -> Double -> [TimeMachine]
increasePriceByPct machines pct = machines & (traversed.price *~ (1+(pct/100)))
