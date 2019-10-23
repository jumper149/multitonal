module Diatonic where

import Note
import Interval

data Mode = Ionian
          | Dorian
          | Phrygian
          | Lydian
          | Mixolydian
          | Aeolian
          | Locrian
  deriving (Read, Show, Eq, Ord, Enum)

data Transition = From1To2
                | From2To3
                | From3To4
                | From4To5
                | From5To6
                | From6To7
                | From7To8
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

-- Specify semitone 'Transition's in order. Rest will be whole steps in the scale.
data ScaleSemisteps = ScaleSemisteps Transition Transition

scaleSemisteps :: Mode -> ScaleSemisteps
scaleSemisteps Ionian     = ScaleSemisteps From3To4 From7To8
scaleSemisteps Dorian     = ScaleSemisteps From2To3 From6To7
scaleSemisteps Phrygian   = ScaleSemisteps From1To2 From5To6
scaleSemisteps Lydian     = ScaleSemisteps From4To5 From7To8
scaleSemisteps Mixolydian = ScaleSemisteps From3To4 From6To7
scaleSemisteps Aeolian    = ScaleSemisteps From2To3 From5To6
scaleSemisteps Locrian    = ScaleSemisteps From1To2 From4To5

type Scale = [Tone]

scale :: Mode -> Tone -> Scale
scale m t = toEnum . (`mod` (fromEnum (maxBound :: Tone) + 1)) . (+ fromEnum t) . fromEnum <$> halfstepsFromRoot
  where halfstepsFromRoot = [ sum . take n $ semisteps | n <- [ 0 .. 6 ] ]
        semisteps = insertSemistep trans2 . insertSemistep trans1 $ wholesteps
        wholesteps = repeat (2 :: HalfSteps)
        (ScaleSemisteps trans1 trans2) = scaleSemisteps m

insertSemistep :: Transition -> [HalfSteps] -> [HalfSteps]
insertSemistep t hs = hd ++ 1 : tl
  where (hd , tl) = splitAt (fromEnum t) hs
