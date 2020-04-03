{-# LANGUAGE FlexibleInstances #-}

module PrettyPrint ( PrettyPrint (..)
                   ) where

import Data.Foldable (toList)

-- | A class for pretty-printing.
class PrettyPrint p where
  pp :: p -> String

instance (PrettyPrint p1, PrettyPrint p2) => PrettyPrint (p1,p2) where
  pp (p1 , p2) = pp p1 <> ", " <> pp p2

instance PrettyPrint p => PrettyPrint (Maybe p) where
  pp (Just p) = pp p
  pp Nothing = ""

instance PrettyPrint p => PrettyPrint [p] where
  pp ls = unlines $ pp <$> ls

newtype PrettyDefault a = PrettyDefault a

instance (Foldable f, Functor f, PrettyPrint p) => PrettyPrint (PrettyDefault (f p)) where
  pp (PrettyDefault fp) = unlines . toList $ pp <$> fp
