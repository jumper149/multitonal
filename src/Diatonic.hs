{-# LANGUAGE RecordWildCards #-}

module Diatonic ( Mode (..)
                , Scale (..)
                , scale
                ) where

import Note
import Interval

import Data.List (sort)

-- | Mode of a diatonic scale.
data Mode = Ionian
          | Dorian
          | Phrygian
          | Lydian
          | Mixolydian
          | Aeolian
          | Locrian
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

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
scaleSemisteps m = ScaleSemisteps fstHs sndHs
  where [ fstHs , sndHs ] = sort $ shift <$> ionianHs
        ionianHs = [ From3To4 , From7To8 ]
        shift = toEnum . limit . (+ (- fromEnum m)) . fromEnum
        limit = (`mod` (fromEnum (maxBound :: Transition) + 1))

-- | Diatonic scale.
data Scale = Scale { s1 :: Note
                   , s2 :: Note
                   , s3 :: Note
                   , s4 :: Note
                   , s5 :: Note
                   , s6 :: Note
                   , s7 :: Note
                   }
  deriving (Read, Eq)

instance Show Scale where
  show s = unwords $ "Scale" : (show <$> tones)
    where tones = [ s1 , s2 , s3 , s4 , s5 , s6 , s7 ] <*> [ s ]

-- | Construct the diatonic 'Scale' from a 'Mode', by giving it's root 'Note'.
scale :: Mode -> Note -> Scale
scale m t = Scale { .. }
  where [s1,s2,s3,s4,s5,s6,s7] = toNote <$> halfstepsFromRoot
        toNote = toEnum . (`mod` (fromEnum (maxBound :: Note) + 1)) . (+ fromEnum t) . fromEnum
        halfstepsFromRoot = [ sum . take n $ semisteps | n <- [ 0 .. 6 ] ]
        semisteps = insertSemistep trans2 . insertSemistep trans1 $ wholesteps
        wholesteps = repeat (2 :: HalfSteps)
        (ScaleSemisteps trans1 trans2) = scaleSemisteps m

insertSemistep :: Transition -> [HalfSteps] -> [HalfSteps]
insertSemistep t hs = hd ++ 1 : tl
  where (hd , tl) = splitAt (fromEnum t) hs
