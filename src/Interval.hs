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

halfStepsFromC :: Tone -> HalfSteps
halfStepsFromC C  = 0
halfStepsFromC CD = 1
halfStepsFromC D  = 2
halfStepsFromC DE = 3
halfStepsFromC E  = 4
halfStepsFromC F  = 5
halfStepsFromC FG = 6
halfStepsFromC G  = 7
halfStepsFromC GA = 8
halfStepsFromC A  = 9
halfStepsFromC AB = 10
halfStepsFromC B  = 11

toHalfSteps :: Interval -> HalfSteps
toHalfSteps = toEnum . fromEnum

fromHalfSteps :: HalfSteps -> Interval
fromHalfSteps n = toEnum . fromEnum $ n `mod` 12

interval :: Tone -> Tone -> Interval
interval x y = fromHalfSteps $ halfStepsFromC y - halfStepsFromC x

halfSteps :: Note -> Note -> HalfSteps
halfSteps (Note n x) (Note m y) = ocHs + hs
  where hs = toHalfSteps $ interval x y
        ocHs = octavesToHalfSteps $ m - n
