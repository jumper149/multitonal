module Chord ( info -- needs rework
             , showInfotain -- for now
             , infotain -- for now
             , tonicTriad
             , tonicTriadTone
             , tonicSeventh
             , tonicSeventhTone
             ) where

import Note
import Interval
import Frequency
import Diatonic

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

newtype Chord = Chord (NE.NonEmpty Note)
  deriving (Read, Eq)

instance Show Chord where
  show (Chord ne) = "Chord " ++ unwords (show <$> NE.toList ne)

newtype Polytone = Polytone (S.Set Tone)
  deriving (Read, Eq)

instance Show Polytone where
  show (Polytone s) = "Polytone " ++ unwords (show <$> S.toList s)

infotain :: [Tone] -> [Tone] -> [((Tone,Tone),(Interval,Cent))]
infotain c1 c2 = info <$> c1 <*> c2

info :: Tone -> Tone -> ((Tone,Tone),(Interval,Cent))
info x y = ((x , y) , (interval x y , correctCent standardTuning x y))

showInfotain :: [((Tone,Tone),(Interval,Cent))] -> String
showInfotain = unlines . map show

tonicTriad :: Mode -> Note -> Chord
tonicTriad m t = Chord $ s1 NE.:| [ s3 , s5 ] <*> pure (scale m t)

tonicTriadTone :: Mode -> Tone -> Polytone
tonicTriadTone m (Tone n t) = Polytone . S.fromList $ rootTone : ascendingTone rootTone rest
  where rootTone = Tone n root
        Chord (root NE.:| rest) = tonicTriad m t

tonicSeventh :: Mode -> Note -> Chord
tonicSeventh m t = Chord $ s1 NE.:| [ s3 , s5 , s7 ] <*> pure (scale m t)

tonicSeventhTone :: Mode -> Tone -> Polytone
tonicSeventhTone m (Tone n t) = Polytone . S.fromList $ rootTone : ascendingTone rootTone rest
  where rootTone = Tone n root
        Chord (root NE.:| rest) = tonicSeventh m t

ascendingTone :: Tone -> [Note] -> [Tone]
ascendingTone _ [] = []
ascendingTone (Tone n prev) (t:ts) = next : ascendingTone next ts
  where next = if Tone n prev <= lower
               then lower
               else higher
        lower = Tone n t
        higher = Tone (n + 1) t
