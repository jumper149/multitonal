{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Frequency ( Frequency
                 , Cent
                 , Tuning
                 , standardTuning
                 , toFrequency
                 , fromFrequency
                 , correctCent -- needs Interval (maybe outsource?)
                 ) where

import Note
import Interval

-- | Frequency in Hertz.
newtype Frequency = Hertz Double
  deriving (Read, Eq, Ord, Num, Fractional, Floating, Real, RealFrac)

instance Show Frequency where
  show f = (show . (round :: Frequency -> Integer)) f ++ "Hz"

-- | Hundreths of semisteps on the chromatic scale.
newtype Cent = Cent Double
  deriving (Read, Eq, Ord, Num, Fractional, Floating, Real, RealFrac)

instance Show Cent where
  show ct = (show . (round :: Cent -> Integer)) ct ++ "ct"

-- | Intonation of each Tone on the chromatic scale.
newtype Tuning = EqualTemperament Frequency -- ^ equal temperament with concert pitch given by the 'Frequency' of @A₄@
  deriving (Read, Show, Eq)

standardTuning :: Tuning
standardTuning = EqualTemperament 440

-- | Return the 'Frequency' of a 'Tone' in a specific 'Tuning'.
toFrequency :: Tuning -> Tone -> Frequency
toFrequency (EqualTemperament a) t = a * 2 ** (hs / 12)
  where hs = fromIntegral $ fromEnum t - fromEnum (A :- 4)

-- | Return the closest 'Tone' and the distance to the actual 'Frequency' in 'Cent'.
fromFrequency :: Tuning -> Frequency -> (Tone , Cent)
fromFrequency (EqualTemperament a) h = (transpose wholeHs $ A :- 4 , Cent $ 100 * missingHs)
  where wholeHs = round hs
        missingHs = hs - fromIntegral wholeHs
        hs = 12 * log quotient / log 2
        Hertz quotient = h / a

-- | Difference to the actual 'Interval', spanned by two 'Tone's in 'Cent's.
correctCent :: Tuning -> Tone -> Tone -> Cent
correctCent t x y = Cent $ 100 * 12 * log quotient / log 2
  where Hertz quotient = xHz / yHz
        xHz = predictFrequency t i x
        yHz = toFrequency t y
        i = (Prime :+:) . toEnum $ fromEnum y - fromEnum x -- NEEDS TO BE CHANGED (mapping to all intervals) TODO

predictFrequency :: Tuning -> Interval -> Tone -> Frequency
predictFrequency t i x = (* toFrequency t x) . fromRational . ratio $ i
