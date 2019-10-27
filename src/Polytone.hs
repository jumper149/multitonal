module Polytone ( fromChord
                ) where

import Note
import Chord

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

newtype Polytone = Polytone (S.Set Tone)
  deriving (Read, Eq)

instance Show Polytone where
  show (Polytone s) = "Polytone " ++ unwords (show <$> S.toList s)

fromChord :: OctaveCount -> Chord -> Polytone
fromChord n c = Polytone . S.fromList $ ascendingTone rootTone rest
  where rootTone = Tone n root
        root NE.:| rest = toNonEmpty c

ascendingTone :: Tone -> [Note] -> [Tone]
ascendingTone prev []     = [ prev ]
ascendingTone prev (n:ns) = prev : ascendingTone next ns
  where next = if prev < lower
               then lower
               else higher
        higher = Tone (i + 1) n
        lower = Tone i n
        Tone i _ = prev
