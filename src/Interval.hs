module Interval where

import Note

type HalfSteps = Integer

octavesToHalfSteps :: OctaveCount -> HalfSteps
octavesToHalfSteps = (12 *)

data Interval = Prime
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
  deriving (Read, Show, Eq, Ord, Enum)

toHalfSteps :: Interval -> HalfSteps
toHalfSteps = toEnum . fromEnum

fromHalfSteps :: HalfSteps -> Interval
fromHalfSteps n = toEnum . fromEnum $ n `mod` 12

intervalTone :: Tone -> Tone -> Interval
intervalTone x y = fromHalfSteps . toEnum $ fromEnum y - fromEnum x

halfSteps :: Note -> Note -> HalfSteps
halfSteps (Note n x) (Note m y) = ocHs + hs
  where hs = toHalfSteps $ intervalTone x y
        ocHs = octavesToHalfSteps $ m - n

interval :: Note -> Note -> Interval
interval x y = fromHalfSteps $ halfSteps x y
