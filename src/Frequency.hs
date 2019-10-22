module Frequency where

import Note
import Interval

newtype Hertz = Hertz Double
  deriving (Read, Show, Eq, Ord)

data Frequency = Frequency Hertz Rational
  deriving (Read, Show, Eq, Ord)

data Tuning = EqualTemperament Hertz Note
  deriving (Read, Show, Eq)

standardTuning :: Tuning
standardTuning = EqualTemperament (Hertz 440) (Note 4 A)

toHertz :: Frequency -> Hertz
toHertz (Frequency (Hertz f) r) = Hertz $ (2**(fromRational r)) * f

fromNote :: Tuning -> Note -> Frequency
fromNote (EqualTemperament f x) y = Frequency f $ (/12) . fromIntegral $ halfSteps x y
