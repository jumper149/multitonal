{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Frequency where

import Note
import Interval

newtype Hertz = Hertz Double
  deriving (Read, Show, Eq, Ord, Num, Fractional, Floating)

data Frequency = Frequency Hertz Rational
               | AddFrequencies Frequency Frequency
  deriving (Read, Show, Eq, Ord)

instance Num Frequency where
  (+) = AddFrequencies

  (*) (Frequency f1 r1) (Frequency f2 r2) = Frequency (f1 * f2) (r1 + r2)
  (*) (AddFrequencies fr1 fr2) fr3 = AddFrequencies (fr1 * fr3) (fr2 * fr3)
  (*) fr1 (AddFrequencies fr2 fr3) = AddFrequencies (fr1 * fr2) (fr1 * fr3)

  negate fr = Frequency (-1) 0 * fr

  signum (Frequency _ r) = Frequency 1 r
  signum (AddFrequencies _ _) = undefined

  abs (Frequency f _) = Frequency f 0
  abs (AddFrequencies _ _) = undefined

  fromInteger i = Frequency (fromInteger i) 0

data Tuning = EqualTemperament Hertz Note
  deriving (Read, Show, Eq)

standardTuning :: Tuning
standardTuning = EqualTemperament (Hertz 440) (Note 4 A)

toHertz :: Frequency -> Hertz
toHertz (Frequency (Hertz f) r) = Hertz $ (2 ** fromRational r) * f
toHertz (AddFrequencies f1 f2) = toHertz f1 + toHertz f2

fromNote :: Tuning -> Note -> Frequency
fromNote (EqualTemperament f x) y = Frequency f $ (/12) . fromIntegral $ halfSteps x y
