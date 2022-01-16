{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Pure.Spacetime.Similar where

import Pure.Spacetime.Magnitude

import Data.Int
import Data.Word

import GHC.Generics

class Similar a where
  sim :: (Real b) => b -> a -> a -> Bool
  default sim :: (Generic a,GSimilar (Rep a),Real b) => b -> a -> a -> Bool
  sim b x y = gsim b (from x) (from y)

simReal :: (Floating a, Real a, Real b) => b -> a -> a -> a
simReal b x y
  | x < 0 && y < 0 = simReal b (abs x) (abs y)
  | x < 0          = simReal b (abs x) (y + x * (-2))
  | y < 0          = simReal b (x + y * (-2)) (abs y)
  | x < 1 && y < 1 = go (x ** (-1)) (y ** (-1))
  | x < 1          = go (x ** (-1)) y
  | y < 1          = go x (y ** (-1))
  | otherwise      = go x y
  where
    go = magReal b

(~=) :: Similar a => a -> a -> Bool
(~=) = sim (exp 1)
infix 4 ~=

similar :: (Similar a) => a -> a -> Bool
similar = sim (exp 1)

instance {-# INCOHERENT #-} (Floating a, Real a) => Similar a where 
  sim b x y = simReal b x y <= 1

class GSimilar a where
  gsim :: (Real b) => b -> a x -> a x -> Bool

instance ( Datatype d, GSimilar a ) => GSimilar ( D1 d a ) where
  gsim b (M1 d1) (M1 d2) = gsim b d1 d2

instance ( Constructor c, GSimilar a ) => GSimilar ( C1 c a ) where
  gsim b (M1 c1) (M1 c2) = gsim b c1 c2

instance ( Selector s, GSimilar a ) => GSimilar ( S1 s a ) where
  gsim b (M1 s1) (M1 s2) = gsim b s1 s2

instance ( Similar a ) => GSimilar ( K1 r a ) where
  gsim b (K1 x) (K1 y) = sim b x y

instance ( GSimilar x, GSimilar y ) => GSimilar (x :+: y) where
  gsim b (L1 x) (L1 y) = gsim b x y
  gsim b (R1 x) (R1 y) = gsim b x y
  gsim _ _ _ = False

instance ( GSimilar x, GSimilar y ) => GSimilar (x :*: y) where
  gsim b (x1 :*: y1) (x2 :*: y2) = gsim b x1 x2 && gsim b y1 y2
