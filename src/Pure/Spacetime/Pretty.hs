{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Pure.Spacetime.Pretty where

class Pretty a where
  pretty :: a -> String

instance Pretty [Char] where
  pretty = id

instance {-# OVERLAPPABLE #-} Show a => Pretty a where
  pretty = show