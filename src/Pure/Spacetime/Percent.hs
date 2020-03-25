{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Pure.Spacetime.Percent where

import Pure.Variance
import Pure.Data.JSON

import Control.Arrow
import Data.Ratio
import GHC.Generics
import Text.Printf

newtype Percent = Percent_ { getPercent :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat,Read,Show,ToJSON,FromJSON)

instance Vary Percent

mkPercent :: (Real a, Real b) => a -> b -> Percent
mkPercent a b =
  let b' = realToFrac b
  in if b' == 0 then
       Percent_ 0
     else
       Percent_ $ realToFrac (round ((realToFrac a / b') * 100)) / 100

pattern Percent :: (Real a, Real b) => a -> b -> Percent
pattern Percent a b <- (((fromInteger . numerator) &&& (fromInteger . denominator)) . toRational -> (a,b)) where
  Percent a b = mkPercent a b
