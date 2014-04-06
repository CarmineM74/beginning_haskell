{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chapter6.Study where

import Data.List
import qualified Data.Map as M

class Ord v => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

-- In order to simplify the following code, working around
-- the limitation that instances can specify only 
-- distinct type variables, we need to use
-- GHC extension: Flexibleinstances
-- data DoubleVector = DoubleVector (Double,Double)

--instance Vector DoubleVector where
--  distance (DoubleVector (a,b)) (DoubleVector (c,d)) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)

instance Vector (Double,Double) where
  distance (a,b) (c,d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)
  centroid lst = (ta/lstLength,tb/lstLength)
    where
      lstLength = fromIntegral $ length lst
      (ta, tb) = foldr (\(a,b) (ta',tb') -> (a+ta',b+tb')) (0.0,0.0) lst

--instance Vector (a,b) where
--  distance a b = 3

--instance (Num a, Num b) => Vector (a,b) where
--  distance x y = 0

-- The following code can be written
-- using multiple parameters class extension
--class Vectorizable e where
--  toVector :: Vector v => e -> v

-- Altough being shorter, the following
-- class declaration leaves no clues on what's
-- the "v" type parameter.
-- One can infer it is some sort of vector
-- by the name of the class, but without
-- any constraing over "v" no other
-- assumptions can be made.
class Vectorizable e v where
  toVector :: e -> v

instance Vectorizable (Double,Double) (Double,Double) where
  toVector = id

clusterAssignmentPhase :: (Vector v, Vectorizable e v) =>  [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points = foldr closerCentroid initialMap points
  where
    initialMap = M.fromList $ zip centroids (repeat [])
    closerCentroid p m = M.adjust (p:) (chosenCentroid p) m
    chosenCentroid p = minimumBy (\x y -> compare (distance x $ toVector p) (distance y $ toVector p)) centroids 

newCentroidsPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v,v)]
newCentroidsPhase m = M.toList $ fmap (centroid . map toVector) m

shouldStop :: (Vector v) => [(v,v)] -> Double -> Bool
shouldStop centroids threshold = foldr f 0.0 centroids < threshold
  where
    f (x,y) s = s + distance x y

kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -- Initial vectors generator
                                        -> Int                -- number of centroids
                                        -> [e]                -- the information
                                        -> Double             -- threshold
                                        -> (Int,[v])          -- new centroids after convergence

kMeans i k points = kMeans' (i k points) points 0

kMeans' :: (Vector v, Vectorizable e v) => [v] -> [e] -> Int -> Double -> (Int,[v])
kMeans' centroids points steps threshold = if shouldStop oldNewCentroids threshold
                                      then (steps,newCentroids)
                                      else kMeans' newCentroids points (steps+1) threshold
                                  where
                                    assignments = clusterAssignmentPhase centroids points
                                    oldNewCentroids = newCentroidsPhase assignments
                                    newCentroids = map snd oldNewCentroids

initializeSimple :: Int -> [e] -> [(Double,Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n-1) v
