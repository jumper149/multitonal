{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Frequency ( Hertz (..) -- (..) necessary?
                 , Cent (..) -- (..) necessary?
                 , Tuning (..)
                 , standardTuning
                 , correctCent
                 , toHertz
                 , fromHertz
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

data IntonatedTone = Intonated Tuning Tone
  deriving (Read, Show)

-- | Intonation of each Tone on the chromatic scale.
newtype Tuning = EqualTemperament Hertz -- ^ equal temperament with concert pitch given by the 'Frequency' of @A₄@
  deriving (Read, Show, Eq)

standardTuning :: Tuning
standardTuning = EqualTemperament $ Hertz 440

toHertz :: IntonatedTone -> Hertz
toHertz (EqualTemperament a `Intonated` t) = a * 2 ** (hs / 12)
  where hs = fromIntegral $ fromEnum t - fromEnum (Tone 4 A)

fromHertz :: Tuning -> Hertz -> (IntonatedTone , Cent)
fromHertz (EqualTemperament a) h = (EqualTemperament a `Intonated` t , ct)
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
        xHz = predictHertz t (interval x y) x
        yHz = toHertz . Intonated t $ y

predictHertz :: Tuning -> Interval -> Tone -> Hertz
predictHertz t i x = (* (toHertz . Intonated t) x) . fromRational . fromInterval $ i
