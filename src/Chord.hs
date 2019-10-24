module Chord ( info -- needs rework
             , showInfotain -- for now
             , infotain -- for now
             ) where

import Note
import Interval
import Frequency

type Chord = [Note]

infotain :: Chord -> Chord -> [((Note,Note),(Interval,Cent))]
infotain c1 c2 = filter f $ info <$> c1 <*> c2
  where f ((x,y),_) = x < y

info :: Note -> Note -> ((Note,Note),(Interval,Cent))
info x y = ((x , y) , (interval x y , correctCent standardTuning x y))

showInfotain :: [((Note,Note),(Interval,Cent))] -> String
showInfotain = unlines . map show
