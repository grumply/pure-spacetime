{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pure.Spacetime.Frequency where

import Pure.Variance
import Pure.Data.JSON

import Pure.Spacetime.Improving
import Pure.Spacetime.Magnitude
import Pure.Spacetime.Base
import Pure.Spacetime.Space
import Pure.Spacetime.Time

import Data.Int

import GHC.Generics

import Text.Printf

class IsFrequency r where
  toFrequency :: Hertz -> r
  fromFrequency :: r -> Hertz

newtype Hertz = Hertz_ { getHertz :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat,Read,Show,ToJSON,FromJSON)

instance Vary Hertz
instance Magnitude Hertz
instance Improving Hertz

instance IsFrequency Hertz where
    toFrequency = id
    fromFrequency = id

viewHertz :: IsTime t => Hertz -> (Double,t)
viewHertz (Hertz r) = (r,Seconds 1)

mkHertz :: (Real a, Real b) => a -> b -> Hertz
mkHertz b t =
  let time = realToFrac t
  in if time == 0 then
       Hertz 0
     else
       Hertz (realToFrac b / time)

pattern Hertz :: (IsFrequency r) => Double -> r
pattern Hertz h <- (getHertz . fromFrequency -> h) where
  Hertz h = toFrequency (Hertz_ h)

pattern Frequency :: (IsTime t, IsFrequency r) => Double -> t -> r
pattern Frequency d t <- (viewHertz . fromFrequency -> (d,t)) where
  Frequency d (Seconds t) = toFrequency $ mkHertz d t

