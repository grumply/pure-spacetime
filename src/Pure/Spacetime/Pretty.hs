{-# LANGUAGE FlexibleInstances #-}
module Pure.Spacetime.Pretty where

class Pretty a where
  pretty :: a -> String

instance Pretty [Char] where
  pretty = id

