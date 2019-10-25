module Info ( info -- rework with new data?
            , mapChord -- scrap?
            where

mapChord :: (Note -> Note) -> Chord -> Chord
mapChord f (Chord ne) = Chord $ f <$> ne

info :: Tone -> Tone -> ((Tone,Tone),(Interval,Cent))
info x y = ((x , y) , (interval x y , correctCent standardTuning x y))
