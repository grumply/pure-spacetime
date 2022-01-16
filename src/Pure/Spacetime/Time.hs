{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pure.Spacetime.Time where

import Pure.Variance
import qualified Pure.Data.Time as Time
import Pure.Data.JSON hiding (pretty)

import Pure.Spacetime.Improving
import Pure.Spacetime.Base
import Pure.Spacetime.Magnitude

import GHC.Generics

import Text.Printf

import Data.Int

newtype Seconds = Seconds_ { getSeconds :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat,Read,Show,ToJSON,FromJSON)
instance Vary Seconds
instance Base Seconds
instance Magnitude Seconds

class IsTime a where
  toTime :: Seconds -> a
  fromTime :: a -> Seconds

instance IsTime Seconds where
  toTime = id
  fromTime = id

instance IsTime Time.Time where
  toTime (Nanoseconds ns) = Time.Nanoseconds (round ns)
  fromTime (Time.Nanoseconds ns) = Nanoseconds (fromIntegral ns)

pattern Hours :: IsTime t => Double -> t
pattern Hours s <- ((/60^2) . getSeconds . fromTime -> s) where
  Hours (toTime . Seconds_ . (*60^2) -> s) = s

pattern Minutes :: IsTime t => Double -> t
pattern Minutes s <- ((/60) . getSeconds . fromTime -> s) where
  Minutes (toTime . Seconds_ . (*60) -> s) = s

pattern Seconds :: IsTime t => Double -> t
pattern Seconds s <- (getSeconds . fromTime -> s) where
  Seconds (toTime . Seconds_ -> s) = s

pattern Milliseconds :: IsTime t => Double -> t
pattern Milliseconds s <- ((* 1000) . getSeconds . fromTime -> s) where
  Milliseconds (toTime . Seconds_ . (/1000) -> s) = s

pattern Microseconds :: IsTime t => Double -> t
pattern Microseconds s <- ((* 1000000) . getSeconds . fromTime -> s) where
  Microseconds (toTime . Seconds_ . (/1000000) -> s) = s

pattern Nanoseconds :: IsTime t => Double -> t
pattern Nanoseconds s <- ((* 1000000000) . getSeconds . fromTime -> s) where
  Nanoseconds (toTime . Seconds_ . (/1000000000) -> s) = s

pattern Picoseconds :: IsTime t => Double -> t
pattern Picoseconds s <- ((* 1000000000000) . getSeconds . fromTime -> s) where
  Picoseconds (toTime . Seconds_ . (/1000000000000) -> s) = s


