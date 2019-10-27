{-# LANGUAGE RecordWildCards #-}

module Diatonic ( Mode (..)
                , Scale (..)
                , scale
                ) where

import Note
import Interval

-- | Mode of a diatonic scale.
data Mode = Ionian
          | Dorian
          | Phrygian
          | Lydian
          | Mixolydian
          | Aeolian
          | Locrian
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

scaleSemisteps :: Mode -> [HalfSteps]
scaleSemisteps m = take (length ionianHs) . drop (fromEnum m) $ cycle ionianHs
  where ionianHs = [ 2 , 2 , 1 , 2 , 2 , 2 , 1 ]

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

instance Transposable Scale where
  transpose n s = Scale { s1 = t s1
                        , s2 = t s2
                        , s3 = t s3
                        , s4 = t s4
                        , s5 = t s5
                        , s6 = t s6
                        , s7 = t s7
                        }
    where t f = transpose n . f $ s

-- | Construct the diatonic 'Scale' from a 'Mode', by giving it's root 'Note'.
scale :: Mode -> Note -> Scale
scale m t = Scale { .. }
  where [s1,s2,s3,s4,s5,s6,s7] = toNote <$> halfstepsFromRoot
        toNote = toEnum . (`mod` (fromEnum (maxBound :: Note) + 1)) . (+ fromEnum t) . fromEnum
        halfstepsFromRoot = [ sum . take n $ semisteps | n <- [ 0 .. length semisteps - 1 ] ]
        semisteps = scaleSemisteps m
