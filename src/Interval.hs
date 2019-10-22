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
  deriving (Read, Show, Eq, Ord)

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
toHalfSteps Prime         = 0
toHalfSteps MinorSecond   = 1
toHalfSteps MajorSecond   = 2
toHalfSteps MinorThird    = 3
toHalfSteps MajorThird    = 4
toHalfSteps PerfectFourth = 5
toHalfSteps Tritone       = 6
toHalfSteps PerfectFifth  = 7 
toHalfSteps MinorSixth    = 8
toHalfSteps MajorSixth    = 9
toHalfSteps MinorSeventh  = 10
toHalfSteps MajorSeventh  = 11
toHalfSteps Octave        = 12

fromHalfSteps :: HalfSteps -> Interval
fromHalfSteps n = case n `mod` 12 of
                    0  -> Prime
                    1  -> MinorSecond
                    2  -> MajorSecond
                    3  -> MinorThird
                    4  -> MajorThird
                    5  -> PerfectFourth
                    6  -> Tritone
                    7  -> PerfectFifth
                    8  -> MinorSixth
                    9  -> MajorSixth
                    10 -> MinorSeventh
                    11 -> MajorSeventh
                    _  -> undefined

interval :: Tone -> Tone -> Interval
interval x y = fromHalfSteps $ halfStepsFromC y - halfStepsFromC x

halfSteps :: Note -> Note -> HalfSteps
halfSteps (Note n x) (Note m y) = ocHs + hs
  where hs = toHalfSteps $ interval x y
        ocHs = octavesToHalfSteps $ m - n
