module KMeans.Vector where

import Vector
import Data.List
import qualified Data.Map as M

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v,v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

clusterAssignmentPhase :: (Ord v, Vector v, Vectorizable e v)
                       => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
    let initialMap = M.fromList $ zip centroids (repeat [])
        in foldr (\p m -> let chosenC = minimumBy (compareDistance p) centroids
                            in M.adjust (p:) chosenC m)
            initialMap points
    where
        compareDistance p x y = compare (distance x $ toVector p)
                                        (distance y $ toVector p)


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
