{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Frequency ( Hertz (..) -- (..) necessary?
                 , Cent (..) -- (..) necessary?
                 , Tuning (..)
                 , standardTuning
                 , correctHertz
                 , correctCent
                 , predictHertz
                 , toHertz
                 , fromTone
                 , fromHertz -- for now
                 , toTone -- for now
                 ) where

import Note
import Interval
import Ratio

-- | Frequency in Hertz.
newtype Hertz = Hertz Double
  deriving (Read, Eq, Ord, Num, Fractional, Floating, Real, RealFrac)

instance Show Hertz where
  show f = (show . (round :: Hertz -> Integer)) f ++ "Hz"

-- | Hundreths of 'HalfSteps'.
newtype Cent = Cent Double
  deriving (Read, Eq, Ord, Num, Fractional, Floating, Real, RealFrac)

instance Show Cent where
  show ct = (show . (round :: Cent -> Integer)) ct ++ "ct"

data Frequency = Frequency Hertz Integer
               | AddFrequencies Frequency Frequency
  deriving (Read, Show, Eq, Ord)

instance Num Frequency where
  (+) = AddFrequencies

  (*) (Frequency f1 h1) (Frequency f2 h2) = Frequency (f1 * f2) (h1 + h2)
  (*) (AddFrequencies fh1 fh2) fh3 = AddFrequencies (fh1 * fh3) (fh2 * fh3)
  (*) fh1 (AddFrequencies fh2 fh3) = AddFrequencies (fh1 * fh2) (fh1 * fh3)

  negate (Frequency f h) = Frequency (-f) h
  negate (AddFrequencies fh1 fh2) = AddFrequencies (negate fh1) (negate fh2)

  signum (Frequency _ h) = Frequency 1 h
  signum (AddFrequencies _ _) = undefined

  abs (Frequency f _) = Frequency f 0
  abs (AddFrequencies _ _) = undefined

  fromInteger i = Frequency (fromInteger i) 0

data Tuning = EqualTemperament Hertz Tone
  deriving (Read, Show, Eq)

standardTuning :: Tuning
standardTuning = EqualTemperament (Hertz 440) (Tone 4 A)

fromTone :: Tuning -> Tone -> Frequency
fromTone (EqualTemperament f x) y = Frequency f $ halfSteps x y

toTone :: Tuning -> Frequency -> Tone
toTone (EqualTemperament f x) (Frequency g h)
  | f == g = toEnum $ fromEnum x + fromEnum h
  | otherwise = undefined
toTone _ _ = undefined

toHertz :: Frequency -> Hertz
toHertz (Frequency f h) = (2 ** (fromIntegral h / 12)) * f
toHertz (AddFrequencies f1 f2) = toHertz f1 + toHertz f2

fromHertz :: Tuning -> Hertz -> (Frequency , Double)
fromHertz (EqualTemperament f _) g = (Frequency f whole , missing)
  where whole = round halfsteps
        missing = halfsteps - fromIntegral whole
        halfsteps = 12 * log quotient / log 2
        Hertz quotient = g / f

-- | Difference to the actual 'Interval', spanned by two 'Tone's in 'Hertz'.
correctHertz :: Tuning -> Tone -> Tone -> Hertz
correctHertz t x y = predictHertz t i x - hertz y
  where hertz = toHertz . fromTone t
        i = interval x y

-- also right
correctCent :: Tuning -> Tone -> Tone -> Cent
correctCent t x y = Cent $ 100 * 12 * log quotient / log 2
  where Hertz quotient = predictHertz t i x / hertz y
        hertz = toHertz . fromTone t
        i = interval x y

predictHertz :: Tuning -> Interval -> Tone -> Hertz
predictHertz t i x = (* (toHertz . fromTone t) x) . fromRational . fromInterval $ i
