{-# LANGUAGE RecordWildCards #-}
module Chapter4.TypeClasses where

import Chapter4.Model

class Priceable p where
  price :: p -> Double

instance Priceable TravelGuide where
  price TravelGuide {..} = tgPrice

instance Priceable Tools where
  price = tPrice

totalPrice :: Priceable p => [p] -> Double
totalPrice = sum . map price

