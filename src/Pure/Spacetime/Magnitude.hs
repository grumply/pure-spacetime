{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Pure.Spacetime.Magnitude where

import Data.Int
import Data.Word

import GHC.Generics

class Magnitude a where
  mag :: Real b => b -> a -> a -> Bool

  default mag :: (Generic a,GMagnitude (Rep a),Real b) => b -> a -> a -> Bool
  mag b x y = gmag b (from x) (from y)

magReal :: (Floating a, Real a, Real b) => b -> a -> a -> a
magReal b x y
  | x < 0 && y < 0 = go (abs x) (abs y)
  | x < 0          = go (abs x) (y + x * (-2))
  | y < 0          = go (x + y * (-2)) (abs y)
  | otherwise      = go x y
  where
    go x y = abs (logBase (fromRational (toRational b)) ((x + 1) / (y + 1)))

(*=) :: Magnitude a => a -> a -> Bool
(*=) = mag (exp 1)
infix 4 *=

magnitude :: Magnitude a => a -> a -> Bool
magnitude = mag (exp 1)

instance {-# INCOHERENT #-} (Floating a, Real a) => Magnitude a where 
  mag b x y = magReal b x y <= 1

class GMagnitude a where
  gmag :: (Real b) => b -> a x -> a x -> Bool

instance ( Datatype d, GMagnitude a ) => GMagnitude ( D1 d a ) where
  gmag b (M1 d1) (M1 d2) = gmag b d1 d2

instance ( Constructor c, GMagnitude a ) => GMagnitude ( C1 c a ) where
  gmag b (M1 c1) (M1 c2) = gmag b c1 c2

instance ( Selector s, GMagnitude a ) => GMagnitude ( S1 s a ) where
  gmag b (M1 s1) (M1 s2) = gmag b s1 s2

instance ( Magnitude a ) => GMagnitude ( K1 r a ) where
  gmag b (K1 x) (K1 y) = mag b x y

instance ( GMagnitude x, GMagnitude y ) => GMagnitude (x :+: y) where
  gmag b (L1 x) (L1 y) = gmag b x y
  gmag b (R1 x) (R1 y) = gmag b x y
  gmag _ _ _ = False

instance ( GMagnitude x, GMagnitude y ) => GMagnitude (x :*: y) where
  gmag b (x1 :*: y1) (x2 :*: y2) = gmag b x1 x2 && gmag b y1 y2
