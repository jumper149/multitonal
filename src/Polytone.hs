module Polytone where

import Note
import Chord

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

newtype Polytone = Polytone (S.Set Tone)
  deriving (Read, Eq)

instance Show Polytone where
  show (Polytone s) = "Polytone " ++ unwords (show <$> S.toList s)

fromChord :: OctaveCount -> Chord -> Polytone
fromChord n (Chord (root NE.:| rest)) = Polytone . S.fromList $ rootTone : ascendingTone rootTone rest
  where rootTone = Tone n root

ascendingTone :: Tone -> [Note] -> [Tone]
ascendingTone _ [] = []
ascendingTone (Tone n prev) (t:ts) = next : ascendingTone next ts
  where next = if Tone n prev <= lower
               then lower
               else higher
        lower = Tone n t
        higher = Tone (n + 1) t
