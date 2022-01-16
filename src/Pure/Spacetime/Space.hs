{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pure.Spacetime.Space where

import Pure.Variance
import Pure.Data.JSON

import Pure.Spacetime.Improving
import Pure.Spacetime.Magnitude
import Pure.Spacetime.Base

import Data.Int

import GHC.Generics

import Text.Printf

class IsSpace a where
  toSpace :: SpaceInBytes -> a
  fromSpace :: a -> SpaceInBytes

newtype SpaceInBytes = SpaceInBytes { getBytes :: Double }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Fractional,Floating,RealFloat,Read,Show,ToJSON,FromJSON)

instance Vary SpaceInBytes

instance Base SpaceInBytes where
  base _ = 2 -- based on my ingrained intuition

instance Magnitude SpaceInBytes where -- intuitionistic; megabytes
  mag b (Megabytes b1) (Megabytes b2) = mag b b1 b2

instance IsSpace SpaceInBytes where
  toSpace = id
  fromSpace = id

pattern Attobytes :: IsSpace b => Double -> b
pattern Attobytes abs <- ((* 1e18) . getBytes . fromSpace -> abs) where
  Attobytes (toSpace . SpaceInBytes . (/ 1e18) -> s) = s

pattern Femtobytes :: IsSpace b => Double -> b
pattern Femtobytes fbs <- ((* 1e15) . getBytes . fromSpace -> fbs) where
  Femtobytes (toSpace . SpaceInBytes . (/ 1e15) -> s) = s

pattern Picobytes :: IsSpace b => Double -> b
pattern Picobytes pbs <- ((* 1e12) . getBytes . fromSpace -> pbs) where
  Picobytes (toSpace . SpaceInBytes . (/ 1e12) -> s) = s

pattern Nanobytes :: IsSpace b => Double -> b
pattern Nanobytes nbs <- ((* 1e9) . getBytes . fromSpace -> nbs) where
  Nanobytes (toSpace . SpaceInBytes . (/ 1e9) -> s) = s

pattern Microbytes :: IsSpace b => Double -> b
pattern Microbytes ubs <- ((* 1e6) . getBytes . fromSpace -> ubs) where
  Microbytes (toSpace . SpaceInBytes . (/ 1e6) -> s) = s

pattern Millibytes :: IsSpace b => Double -> b
pattern Millibytes mbs <- ((* 1e3) . getBytes . fromSpace -> mbs) where
  Millibytes (toSpace . SpaceInBytes . (/ 1e3) -> s) =s

pattern Bytes :: IsSpace b => Double -> b
pattern Bytes bs <- (getBytes . fromSpace -> bs) where
  Bytes (toSpace . SpaceInBytes -> s) = s

pattern Kibibytes :: IsSpace b => Double -> b
pattern Kibibytes bs <- ((/ 1e3) . getBytes . fromSpace -> bs) where
  Kibibytes (toSpace . SpaceInBytes . (*1e3) -> s) = s

pattern Kilobytes :: IsSpace b => Double -> b
pattern Kilobytes kbs <- ((/ 2^10) . getBytes . fromSpace -> kbs) where
  Kilobytes (toSpace . SpaceInBytes . (*2^10) -> s) = s

pattern Mebibytes :: IsSpace b => Double -> b
pattern Mebibytes bs <- ((/ 1e6) . getBytes . fromSpace -> bs) where
  Mebibytes (toSpace . SpaceInBytes . (*1e6) -> s) = s

pattern Megabytes :: IsSpace b => Double -> b
pattern Megabytes mbs <- ((/ 2^20) . getBytes . fromSpace -> mbs) where
  Megabytes (toSpace . SpaceInBytes . (*2^20) -> s) = s

pattern Gibibytes :: IsSpace b => Double -> b
pattern Gibibytes bs <- ((/ 1e9) . getBytes . fromSpace -> bs) where
  Gibibytes (toSpace . SpaceInBytes . (*1e9) -> s) = s

pattern Gigabytes :: IsSpace b => Double -> b
pattern Gigabytes gbs <- ((/ 2^30) . getBytes . fromSpace -> gbs) where
  Gigabytes (toSpace . SpaceInBytes . (*2^30) -> s) = s

pattern Tebibytes :: IsSpace b => Double -> b
pattern Tebibytes bs <- ((/ 1e12) . getBytes . fromSpace -> bs) where
  Tebibytes (toSpace . SpaceInBytes . (*1e12) -> s) = s

pattern Terabytes :: IsSpace b => Double -> b
pattern Terabytes tbs <- ((/ 2^40) . getBytes . fromSpace -> tbs) where
  Terabytes (toSpace . SpaceInBytes . (*2^40) -> s) = s

pattern Pebibytes :: IsSpace b => Double -> b
pattern Pebibytes bs <- ((/ 1e15) . getBytes . fromSpace -> bs) where
  Pebibytes (toSpace . SpaceInBytes . (*1e15) -> s) = s

pattern Petabytes :: IsSpace b => Double -> b
pattern Petabytes tbs <- ((/ 2^50) . getBytes . fromSpace -> tbs) where
  Petabytes (toSpace . SpaceInBytes . (*2^50) -> s) = s

pattern Exbibytes :: IsSpace b => Double -> b
pattern Exbibytes bs <- ((/ 1e18) . getBytes . fromSpace -> bs) where
  Exbibytes (toSpace . SpaceInBytes . (*1e18) -> s) = s

pattern Exabytes :: IsSpace b => Double -> b
pattern Exabytes tbs <- ((/ 2^60) . getBytes . fromSpace -> tbs) where
  Exabytes (toSpace . SpaceInBytes . (*2^60) -> s) = s

pattern Zebibytes :: IsSpace b => Double -> b
pattern Zebibytes bs <- ((/ 1e21) . getBytes . fromSpace -> bs) where
  Zebibytes (toSpace . SpaceInBytes . (*1e21) -> s) = s

pattern Zettabytes :: IsSpace b => Double -> b
pattern Zettabytes tbs <- ((/ 2^70) . getBytes . fromSpace -> tbs) where
  Zettabytes (toSpace . SpaceInBytes . (*2^70) -> s) = s

pattern Yobibytes :: IsSpace b => Double -> b
pattern Yobibytes bs <- ((/ 1e24) . getBytes . fromSpace -> bs) where
  Yobibytes (toSpace . SpaceInBytes . (*1e24) -> s) = s

pattern Yottabytes :: IsSpace b => Double -> b
pattern Yottabytes tbs <- ((/ 2^80) . getBytes . fromSpace -> tbs) where
  Yottabytes (toSpace . SpaceInBytes . (*2^80) -> s) = s


