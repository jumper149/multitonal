module Chord where

import Note
import Interval
import Frequency

import Data.List (intercalate)

type Chord = [Note]

info :: Chord -> Chord -> [((Note,Note),(Interval,Hertz))]
info c1 c2 = filter f $ track <$> c1 <*> c2
  where f ((x,y),_) = x < y

track :: Note -> Note -> ((Note,Note),(Interval,Hertz))
track x y = ((x , y) , (interval x y , correct standardTuning x y))

----------------------------

chordC4Maj :: Chord
chordC4Maj = Note 4 <$> [ C , E , G ]

chordA4Maj :: Chord
chordA4Maj = Note 4 <$> [ A , C , E ]

showInfo :: [((Note,Note),(Interval,Hertz))] -> String
showInfo = intercalate "\n" . map show
