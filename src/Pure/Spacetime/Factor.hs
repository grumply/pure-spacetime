{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Pure.Spacetime.Factor where

import Pure.Data.JSON
import Pure.Variance

import Pure.Spacetime.Base
import Pure.Spacetime.Magnitude
import Pure.Spacetime.Similar
import Pure.Spacetime.Pretty

import GHC.Generics

import Text.Printf

newtype SomeFactor = SomeFactor { getFactor :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat,Read,Show,ToJSON,FromJSON)
instance Vary SomeFactor
instance Base SomeFactor
instance Similar SomeFactor
instance Magnitude SomeFactor

instance Pretty SomeFactor where
    pretty (SomeFactor f) = printf "%.2fx" f

class IsFactor f where
  toFactor :: SomeFactor -> f
  fromFactor :: f -> SomeFactor

instance IsFactor SomeFactor where
  toFactor = id
  fromFactor = id

viewFactor :: (Fractional a,Real b,IsFactor f) => f -> (a,b)
viewFactor (fromFactor -> SomeFactor f) = (realToFrac f,1)

mkFactor :: (Real a,Real b,IsFactor f) => a -> b -> f
mkFactor a 0 = toFactor $ SomeFactor 0
mkFactor a b = toFactor $ SomeFactor $ realToFrac a / realToFrac b

pattern Factor :: (Real a, Fractional a, Real b, IsFactor f) => a -> b -> f
pattern Factor a b <- (viewFactor -> (a,b)) where
  Factor a b = mkFactor a b
