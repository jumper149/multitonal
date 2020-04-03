{-# LANGUAGE FlexibleInstances #-}

module PrettyPrint ( PrettyPrint (..)
                   ) where

import Data.Foldable (toList)

-- | A class for pretty-printing.
class PrettyPrint p where
    pp :: p -> String

instance (Foldable f, Functor f, PrettyPrint p) => PrettyPrint (f p) where
    pp fp = unlines . toList $ pp <$> fp
