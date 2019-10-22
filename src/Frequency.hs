module Frequency where

import Note
import Interval

type Hertz = Double

data Frequency = Frequency Rational Hertz
  deriving (Read, Show, Eq, Ord)

data Tuning = EqualTemperament Note Hertz
  deriving (Read, Show, Eq)

standardTuning :: Tuning
standardTuning = EqualTemperament (Note A 4) 440

toHertz :: Frequency -> Double
toHertz (Frequency r f) = (2**(fromRational r)) * f

fromNote :: Tuning -> Note -> Frequency
fromNote (EqualTemperament x f) y = Frequency m f
  where m = (/12) . fromIntegral $ halfSteps x y
