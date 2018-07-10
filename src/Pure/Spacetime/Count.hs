{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pure.Spacetime.Count where

import Pure.Data.JSON hiding (pretty)
import Pure.Variance

import Pure.Spacetime.Improving
import Pure.Spacetime.Pretty
import Pure.Spacetime.Similar
import Pure.Spacetime.Magnitude
import Pure.Spacetime.Base

import Pure.Spacetime.Space
import Pure.Spacetime.Time

import Data.Int

import GHC.Generics

import Text.Printf

newtype SomeCount = SomeCount { getCount :: Double }
  deriving (Generic,Eq,Ord,Num,Read,Show,Real,Floating,Fractional,RealFrac,Base,Similar,ToJSON,FromJSON,Magnitude)

instance Vary SomeCount

pattern Count :: IsCount c => Double -> c
pattern Count c <- (getCount . fromCount -> c) where
  Count c = toCount (SomeCount c)

instance Pretty SomeCount where
  pretty (SomeCount c) = printf "%.0f" c

class IsCount c where
  toCount :: SomeCount -> c
  fromCount :: c -> SomeCount

instance IsCount SomeCount where
  toCount = id
  fromCount = id

newtype ByteUsageSamples = ByteUsageSamples { getByteUsageSamples :: SomeCount }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,Floating,Fractional,RealFrac,Base,Similar,ToJSON,FromJSON)

instance IsCount ByteUsageSamples where
  toCount = ByteUsageSamples
  fromCount (ByteUsageSamples bus) = bus

----------------------------------------
-- Collection Count

newtype Collections = Collections { getCollections :: SomeCount }
  deriving (Generic,Eq,Ord,Num,Real,Floating,Fractional,RealFrac,Read,Show,ToJSON,FromJSON,Base,Magnitude,Similar)

instance Vary Collections

instance IsCount Collections where
  toCount = Collections
  fromCount = getCollections

instance Pretty Collections where
  pretty (Collections c) = pretty c

instance Improving Collections where
  improving c1 c2 = c1 > c2
  improvingShow _ = ">"
