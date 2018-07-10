{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Pure.Spacetime.Percent where

import Pure.Variance
import Pure.Data.JSON

import Pure.Spacetime.Pretty

import GHC.Generics

import Text.Printf

newtype Percent = Percent { getPercent :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat,Read,Show,ToJSON,FromJSON)

instance Vary Percent

instance Pretty Percent where
    pretty (Percent r)
      | isNaN r   = "0%"
      | otherwise = printf "%.2f%%" (100 * r)

mkPercent :: (Real a, Real b) => a -> b -> Percent
mkPercent a b =
  let b' = realToFrac b
  in if b' == 0 then
       Percent 0
     else
       Percent $ realToFrac (round ((realToFrac a / b') * 100)) / 100
