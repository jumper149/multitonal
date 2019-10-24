module Chord ( info -- needs rework
             , showInfo -- for now
             ) where

import Note
import Interval
import Frequency

type Chord = [Note]

info :: Chord -> Chord -> [((Note,Note),(Interval,Cent))]
info c1 c2 = filter f $ track <$> c1 <*> c2
  where f ((x,y),_) = x < y

track :: Note -> Note -> ((Note,Note),(Interval,Cent))
track x y = ((x , y) , (interval x y , correctCent standardTuning x y))

showInfo :: [((Note,Note),(Interval,Cent))] -> String
showInfo = unlines . map show