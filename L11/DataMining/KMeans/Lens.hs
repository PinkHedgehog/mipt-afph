{-# LANGUAGE TemplateHaskell #-}
module KMeans.Lens where

import Vector
import Data.List
import qualified Data.Map as M
import Control.Lens

data KMeansState e v = KMeansState
                     { _centroids :: [v]
                     , _points    :: [e]
                     , _err       :: Double
                     , _threshold :: Double
                     , _steps     :: Int
                     }

makeLenses ''KMeansState

initializeState :: (Int -> [e] -> [v])
                -> Int -> [e] -> Double -> KMeansState e v
initializeState i n pts t = KMeansState (i n pts) pts (1.0/0.0) t 0

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v,v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)


{-
 - clusterAssignmentPhase :: (Ord v, Vector v, Vectorizable e v
 -                        => [v] -> [e] -> M.Map v [e]
 -}
clusterAssignmentPhase :: (Ord v, Vector v, Vectorizable e v)
                       => KMeansState e v -> M.Map v [e]
clusterAssignmentPhase = error "Not implemented yet!"

shouldStop :: Vector v => [(v, v)] -> Double -> Bool
shouldStop centroids threshold = 
    foldr (\(x, y) s -> s + distance x y) 0.0 centroids < threshold

kMeans :: (Vector v, Vectorizable e v)
       => (Int -> [e] -> [v]) -- initialization function
       -> Int                 -- number of centroids
       -> [e]                 -- the information
       -> Double              -- threshold
       -> [v]                 -- centroids after convergence
kMeans i k points = kMeans' (i k points) points


kMeans' :: (Vector v, Vectorizable e v)
        => [v] -> [e] -> Double -> [v]
kMeans' centroids points threshold =
    let assignments = clusterAssignmentPhase centroids points
        oldNewCentroids = newCentroidPhase assignments
        newCentroids = map snd oldNewCentroids
        in 
            if shouldStop oldNewCentroids threshold
            then newCentroids
            else kMeans' newCentroids points threshold

initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n-1) v
