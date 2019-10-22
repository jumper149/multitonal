{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Frequency where

import Note
import Interval

newtype Hertz = Hertz Double
  deriving (Read, Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac)

data Frequency = Frequency Hertz Integer
               | AddFrequencies Frequency Frequency
  deriving (Read, Show, Eq, Ord)

instance Num Frequency where
  (+) = AddFrequencies

  (*) (Frequency f1 h1) (Frequency f2 h2) = Frequency (f1 * f2) (h1 + h2)
  (*) (AddFrequencies fh1 fh2) fh3 = AddFrequencies (fh1 * fh3) (fh2 * fh3)
  (*) fh1 (AddFrequencies fh2 fh3) = AddFrequencies (fh1 * fh2) (fh1 * fh3)

  negate fh = Frequency (-1) 0 * fh

  signum (Frequency _ h) = Frequency 1 h
  signum (AddFrequencies _ _) = undefined

  abs (Frequency f _) = Frequency f 0
  abs (AddFrequencies _ _) = undefined

  fromInteger i = Frequency (fromInteger i) 0

data Tuning = EqualTemperament Hertz Note
  deriving (Read, Show, Eq)

standardTuning :: Tuning
standardTuning = EqualTemperament (Hertz 440) (Note 4 A)

toHertz :: Frequency -> Hertz
toHertz (Frequency f h) = (2 ** (fromIntegral h / 12)) * f
toHertz (AddFrequencies f1 f2) = toHertz f1 + toHertz f2

fromNote :: Tuning -> Note -> Frequency
fromNote (EqualTemperament f x) y = Frequency f $ halfSteps x y

--toNote :: Tuning -> Frequency -> Note
--toNote (EqualTemperament f x) (Frequency g h)
--  | f == g = undefined
--  | otherwise = undefined

closestFrequency :: Tuning -> Hertz -> (Frequency , Double)
closestFrequency (EqualTemperament f _) g = (Frequency f whole , missing)
  where whole = round halfsteps
        missing = halfsteps - fromIntegral whole
        halfsteps = 12 * log quotient / log 2
        Hertz quotient = g / f
