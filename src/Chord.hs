module Chord ( info -- needs rework
             , showInfotain -- for now
             , infotain -- for now
             , tonicTriad
             , tonicTriadNote
             , tonicSeventh
             , tonicSeventhNote
             ) where

import Note
import Interval
import Frequency
import Diatonic

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

newtype Chord = Chord (NE.NonEmpty Tone)
  deriving (Read, Eq)

instance Show Chord where
  show (Chord ne) = "Chord " ++ unwords (show <$> NE.toList ne)

newtype Polytone = Polytone (S.Set Note)
  deriving (Read, Eq)

instance Show Polytone where
  show (Polytone s) = "Polytone " ++ unwords (show <$> S.toList s)

infotain :: [Note] -> [Note] -> [((Note,Note),(Interval,Cent))]
infotain c1 c2 = info <$> c1 <*> c2

info :: Note -> Note -> ((Note,Note),(Interval,Cent))
info x y = ((x , y) , (interval x y , correctCent standardTuning x y))

showInfotain :: [((Note,Note),(Interval,Cent))] -> String
showInfotain = unlines . map show

tonicTriad :: Mode -> Tone -> Chord
tonicTriad m t = Chord $ s1 NE.:| [ s3 , s5 ] <*> pure (scale m t)

tonicTriadNote :: Mode -> Note -> Polytone
tonicTriadNote m (Note n t) = Polytone . S.fromList $ rootNote : ascendingNote rootNote rest
  where rootNote = Note n root
        Chord (root NE.:| rest) = tonicTriad m t

tonicSeventh :: Mode -> Tone -> Chord
tonicSeventh m t = Chord $ s1 NE.:| [ s3 , s5 , s7 ] <*> pure (scale m t)

tonicSeventhNote :: Mode -> Note -> Polytone
tonicSeventhNote m (Note n t) = Polytone . S.fromList $ rootNote : ascendingNote rootNote rest
  where rootNote = Note n root
        Chord (root NE.:| rest) = tonicSeventh m t

ascendingNote :: Note -> [Tone] -> [Note]
ascendingNote _ [] = []
ascendingNote (Note n prev) (t:ts) = next : ascendingNote next ts
  where next = if Note n prev <= lower
               then lower
               else higher
        lower = Note n t
        higher = Note (n + 1) t
