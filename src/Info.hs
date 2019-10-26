module Info ( info -- rework with new data?
            where

info :: Tone -> Tone -> ((Tone,Tone),(Interval,Cent))
info x y = ((x , y) , (interval x y , correctCent standardTuning x y))
