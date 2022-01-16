{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Pure.Spacetime.Multiplier where

import Pure.Data.JSON
import Pure.Variance

import Pure.Spacetime.Base
import Pure.Spacetime.Magnitude

import GHC.Generics

import Text.Printf

newtype Multiplier = Multiplier_ { getMultiplier :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat,Read,Show,ToJSON,FromJSON)
instance Vary Multiplier
instance Base Multiplier
instance Magnitude Multiplier

class IsMultiplier f where
  toMultiplier :: Multiplier -> f
  fromMultiplier :: f -> Multiplier

instance IsMultiplier Multiplier where
  toMultiplier = id
  fromMultiplier = id

viewMultiplier :: (Fractional a,Real b,IsMultiplier f) => f -> (a,b)
viewMultiplier (fromMultiplier -> Multiplier_ f) = (realToFrac f,1)

mkMultiplier :: (Real a,Real b,IsMultiplier f) => a -> b -> f
mkMultiplier a 0 = toMultiplier $ Multiplier_ 0
mkMultiplier a b = toMultiplier $ Multiplier_ $ realToFrac a / realToFrac b

pattern Multiplier :: (Real a, Fractional a, Real b, IsMultiplier f) => a -> b -> f
pattern Multiplier a b <- (viewMultiplier -> (a,b)) where
  Multiplier a b = mkMultiplier a b
