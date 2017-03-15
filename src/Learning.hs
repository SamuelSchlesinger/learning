{-# LANGUAGE FlexibleContexts #-}

module Learning
    ( ols
    , gls
    , kmeans
    , Centroids
    , Clusters
    , Clustering
    , Initializer
    , average
    , invertMap
    ) where

import Debug.Trace
import Data.Set
import Data.Map.Strict
import Numeric.LinearAlgebra hiding ((!))

--covariance :: (Field n, Ord n, Num (Vector n))
--           => Set (Vector n)
--           -> Matrix n

--covariance set = exxt - uut where
--  exxt = mean $ Prelude.map (\a -> asColumn a <> asRow a) (Data.Set.elems set)
--  uut  = asColumn mu <> asRow mu
--  mu   = average set

-- | Ordinary least squares
ols :: (Field n, Monoid (Matrix n))
    => Matrix n
    -> Vector n
    -> Vector n

ols x y = head $ toColumns $ optimiseMult [l', tr l', tr x, asColumn y] where
  xTx = mTm x
  l   = chol xTx
  l'  = inv l

-- | Generalized least squares
gls :: (Eq n, Num (Vector n), Monoid (Matrix n), Field n) 
    => Matrix n 
    -> Matrix n 
    -> Vector n 
    -> Vector n

gls c x y = t where
  x' = tr x
  ic = inv c
  r  = inv (optimiseMult [x', ic, x])
  s  = optimiseMult [r, x, ic]
  t  = s #> y

type Centroids n = Set (Vector n)

type Clusters n = Map (Vector n) (Set (Vector n))

type Clustering n = (Centroids n, Clusters n)

type Initializer m n = Int -> Set (Vector n) -> m (Centroids n)

-- | KMeans clustering algorithm
kmeans :: (Functor m, Container Vector n, Ord n, Num (Vector n), Normed (Vector n), Field n)
       => Initializer m n
       -> Int
       -> Set (Vector n)
       -> m (Clustering n)

-- KMeans runs by first getting an original clustering, then reclustering
-- until a fixed point is reached.
kmeans init k set = (untilEqual fst (recluster k) . (\centroids -> (centroids, reassign centroids set))) <$> init k set

untilEqual :: Eq a => (e -> a) -> (e -> e) -> e -> e
untilEqual g f e = if g e' == g e then e else untilEqual g f e' where
  e' = f e

-- Reclustering means taking each cluster, computing a new centroid based on
-- the contents, and then reassigning each vector to the cluster which has a
-- centroid closest to it.
recluster :: (Container Vector n, Ord n, Fractional n, Normed (Vector n), Num (Vector n), Field n)
          => Int 
          -> Clustering n 
          -> Clustering n
recluster k (centroids, clusters) = (centroids', reassign centroids' (Data.Set.unions $ Data.Map.Strict.elems clusters)) where
  centroids' = Data.Set.map (\centroid -> average (clusters ! centroid)) centroids

average :: (Container c n, Fractional n, Ord n, Num (c n), Linear n c)
        => Set (c n) -> c n

average set = scale (1 / (fromInteger . toInteger $ Data.Set.size set)) $ Data.Set.foldr (+) one set' where
  (one, set') = Data.Set.deleteFindMin set

-- Reassignment goes like this: given a set of vectors and a set of centroids,
-- return a map from centroid to sets of vectors which have been found to be within this
-- centroid.
reassign :: (Normed (Vector n), Ord n, Eq n, Num (Vector n), Field n)
         => Set (Vector n)
         -> Set (Vector n)
         -> Clusters n

reassign centroids points = invertMap $ fromSet (getClosest centroids) points

-- | Given a map from x to y, returns a map from y to the preimage of y in the
--   original map
invertMap :: (Ord y, Ord x) => Map x y -> Map y (Set x)
invertMap map = st where
  st = fromListWith Data.Set.union [ (e, Data.Set.singleton k) 
                                   | k <- ks, e <- es, map ! k == e ]
  es = Data.Map.Strict.elems map
  ks = keys map

getClosest :: (Normed (Vector n), Num (Vector n), Field n, Eq n) => Set (Vector n) -> Vector n -> Vector n
getClosest points point = if Data.Set.null points then error "getClosest" else fst $ Data.Set.foldr 
  (\candidate (c, d) -> let d' = norm_2 (candidate - point)
                        in if d > d' then (candidate, d') else (c, d))
  (candidate, norm_2 (candidate - point))
   points' where
  (candidate, points') = Data.Set.deleteFindMin points
