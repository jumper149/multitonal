{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Frequency ( Frequency
                 , Cent
                 , Tuning (..)
                 , standardTuning
                 , correctCent
                 , toFrequency
                 , fromFrequency
                 ) where

import Note
import Interval
import Ratio

-- | Frequency in Hertz.
newtype Frequency = Hertz Double
  deriving (Read, Eq, Ord, Num, Fractional, Floating, Real, RealFrac)

instance Show Frequency where
  show f = (show . (round :: Frequency -> Integer)) f ++ "Hz"

-- | Hundreths of 'HalfSteps'.
newtype Cent = Cent Double
  deriving (Read, Eq, Ord, Num, Fractional, Floating, Real, RealFrac)

instance Show Cent where
  show ct = (show . (round :: Cent -> Integer)) ct ++ "ct"

data IntonatedTone = Intonated Tuning Tone
  deriving (Read, Show)

-- | Intonation of each Tone on the chromatic scale.
newtype Tuning = EqualTemperament Frequency -- ^ equal temperament with concert pitch given by the 'Frequency' of @A₄@
  deriving (Read, Show, Eq)

standardTuning :: Tuning
standardTuning = EqualTemperament 440

toFrequency :: IntonatedTone -> Frequency
toFrequency (EqualTemperament a `Intonated` t) = a * 2 ** (hs / 12)
  where hs = fromIntegral $ fromEnum t - fromEnum (Tone 4 A)

-- | Return the closest 'IntonatedTone' and the distance to the actual 'Frequency' in 'Cent'
fromFrequency :: Tuning -> Frequency -> (IntonatedTone , Cent)
fromFrequency (EqualTemperament a) h = (EqualTemperament a `Intonated` t , ct)
  where t = transpose wholeHs $ Tone 4 A
        ct = Cent . (100 *) $ missingHs
        wholeHs = round hs
        missingHs = hs - fromIntegral wholeHs
        hs = 12 * log quotient / log 2
        Hertz quotient = h / a

-- | Difference to the actual 'Interval', spanned by two 'Tone's in 'Cent's.
correctCent :: Tuning -> Tone -> Tone -> Cent
correctCent t x y = Cent $ 100 * 12 * log quotient / log 2
  where Hertz quotient = xHz / yHz
        xHz = predictFrequency t (interval x y) x
        yHz = toFrequency . Intonated t $ y

predictFrequency :: Tuning -> Interval -> Tone -> Frequency
predictFrequency t i x = (* (toFrequency . Intonated t) x) . fromRational . fromInterval $ i
