{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pure.Spacetime.Count where

import Pure.Data.JSON hiding (pretty)
import Pure.Variance

import Pure.Spacetime.Improving
import Pure.Spacetime.Similar
import Pure.Spacetime.Magnitude
import Pure.Spacetime.Base

import Pure.Spacetime.Space
import Pure.Spacetime.Time

import Data.Int

import GHC.Generics

import Text.Printf

newtype Count = Count_ { getCount :: Double }
  deriving (Generic,Eq,Ord,Num,Read,Show,Real,Floating,Fractional,RealFrac,Base,Similar,ToJSON,FromJSON,Magnitude)

instance Vary Count

pattern Count :: IsCount c => Double -> c
pattern Count c <- (getCount . fromCount -> c) where
  Count c = toCount (Count_ c)

class IsCount c where
  toCount :: Count -> c
  fromCount :: c -> Count

instance IsCount Count where
  toCount = id
  fromCount = id

