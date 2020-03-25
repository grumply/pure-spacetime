{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
module Pure.Spacetime (module Pure.Spacetime, module Export) where


import Pure.Variance
import Pure.Data.JSON

import Pure.Spacetime.Base as Export
import Pure.Spacetime.Count as Export
import Pure.Spacetime.Multiplier as Export
import Pure.Spacetime.Improving  as Export
import Pure.Spacetime.Magnitude as Export
import Pure.Spacetime.Percent as Export
import Pure.Spacetime.Frequency as Export
import Pure.Spacetime.Similar as Export
import Pure.Spacetime.Space as Export
import Pure.Spacetime.Time as Export

import Data.Int

import GHC.Generics

import Text.Printf

class IsDataRate r where
  toDataRate :: BytesPerSecond -> r
  fromDataRate :: r -> BytesPerSecond

newtype BytesPerSecond = BytesPerSecond { getDataRate :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat,Read,Show,ToJSON,FromJSON)

instance Vary BytesPerSecond

instance Improving BytesPerSecond

instance Magnitude BytesPerSecond

instance Similar BytesPerSecond

instance IsDataRate BytesPerSecond where
  toDataRate = id
  fromDataRate = id

viewDataRate :: (IsDataRate r, IsSpace s, IsTime t) => r -> (s,t)
viewDataRate (fromDataRate -> BytesPerSecond r) = (Bytes r,Seconds 1)

mkDataRate :: (IsDataRate r, IsSpace s, IsTime t) => s -> t -> r
mkDataRate s t =
  let time = realToFrac (fromTime t)
  in toDataRate $
       if time == 0 then
         BytesPerSecond 0
       else
         BytesPerSecond (realToFrac (fromSpace s) / time)

pattern DataRate :: (IsSpace s, IsTime t, IsDataRate r) => s -> t -> r
pattern DataRate s t <- (viewDataRate -> (s,t)) where
  DataRate s t = mkDataRate s t
