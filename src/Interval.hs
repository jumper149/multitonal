module Interval where

import Note

type HalfSteps = Integer

octavesToHalfSteps :: OctaveCount -> HalfSteps
octavesToHalfSteps = (12 *)

data SimpleInterval = Prime
                    | MinorSecond
                    | MajorSecond
                    | MinorThird
                    | MajorThird
                    | PerfectFourth
                    | Tritone
                    | PerfectFifth
                    | MinorSixth
                    | MajorSixth
                    | MinorSeventh
                    | MajorSeventh
                    | Octave
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

data Interval = Compound OctaveCount SimpleInterval
  deriving (Read, Show, Eq, Ord)

instance Enum Interval where
  fromEnum (Compound n si) = fromEnum n * fromEnum (maxBound :: SimpleInterval) + fromEnum si

  toEnum i = Compound (toEnum n) (toEnum si)
    where maxSimple = maxBound :: SimpleInterval
          (n , si) = i `divMod` fromEnum maxSimple

toHalfSteps :: Interval -> HalfSteps
toHalfSteps = toEnum . fromEnum

fromHalfSteps :: HalfSteps -> Interval
fromHalfSteps = toEnum . fromEnum

intervalTone :: Tone -> Tone -> Interval
intervalTone x y = fromHalfSteps . toEnum $ fromEnum y - fromEnum x

halfSteps :: Note -> Note -> HalfSteps
halfSteps (Note n x) (Note m y) = ocHs + hs
  where hs = toHalfSteps $ intervalTone x y
        ocHs = octavesToHalfSteps $ m - n

interval :: Note -> Note -> Interval
interval x y = fromHalfSteps $ halfSteps x y
