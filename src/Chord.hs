module Chord ( Chord (..) -- necessary?
             , info -- needs rework
             , infotain -- for now
             , tonicTriad
             , tonicSeventh
             , mapChord -- good idea?
             ) where

import Note
import Interval
import Frequency
import Diatonic

import qualified Data.List.NonEmpty as NE

newtype Chord = Chord (NE.NonEmpty Note)
  deriving (Read, Eq)

instance Show Chord where
  show (Chord ne) = "Chord " ++ unwords (show <$> NE.toList ne)

mapChord :: (Note -> Note) -> Chord -> Chord
mapChord f (Chord ne) = Chord $ f <$> ne

infotain :: [Tone] -> [Tone] -> [((Tone,Tone),(Interval,Cent))]
infotain c1 c2 = info <$> c1 <*> c2

info :: Tone -> Tone -> ((Tone,Tone),(Interval,Cent))
info x y = ((x , y) , (interval x y , correctCent standardTuning x y))

tonicTriad :: Mode -> Note -> Chord
tonicTriad m t = Chord $ s1 NE.:| [ s3 , s5 ] <*> pure (scale m t)

tonicSeventh :: Mode -> Note -> Chord
tonicSeventh m t = Chord $ s1 NE.:| [ s3 , s5 , s7 ] <*> pure (scale m t)
