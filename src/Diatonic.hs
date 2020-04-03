{-# LANGUAGE RecordWildCards #-}

module Diatonic ( Mode (..)
                , Scale (..)
                , scale
                ) where

import Note
import PrettyPrint

-- | Mode of a diatonic scale.
data Mode = Ionian
          | Dorian
          | Phrygian
          | Lydian
          | Mixolydian
          | Aeolian
          | Locrian
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance PrettyPrint Mode where
  pp = show

-- | Diatonic scale.
data Scale = Scale { s1 :: Note
                   , s2 :: Note
                   , s3 :: Note
                   , s4 :: Note
                   , s5 :: Note
                   , s6 :: Note
                   , s7 :: Note
                   }
  deriving (Eq, Read, Show)

instance PrettyPrint Scale where
  pp s = unwords $ "Scale" : (pp <$> listNotes s)

instance NoteContainer Scale where
  mapNotes f s = Scale { s1 = f $ s1 s
                       , s2 = f $ s2 s
                       , s3 = f $ s3 s
                       , s4 = f $ s4 s
                       , s5 = f $ s5 s
                       , s6 = f $ s6 s
                       , s7 = f $ s7 s
                       }
  listNotes s = [ s1 , s2 , s3 , s4 , s5 , s6 , s7 ] <*> pure s

instance Transposable Scale where
  transpose n = mapNotes $ transpose n

-- | Construct the diatonic 'Scale' from a 'Mode', by giving it's root 'Note'.
scale :: Mode -> Note -> Scale
scale m t = Scale { .. }
  where [s1,s2,s3,s4,s5,s6,s7] = transpose <$> halfstepsFromRoot <*> pure t
        halfstepsFromRoot = [ sum . take n $ semisteps | n <- [ 0 .. length semisteps - 1 ] ]
        semisteps = scaleSemisteps m

-- | Returns a list of semisteps that refer to the distances between 'Scale' degrees.
scaleSemisteps :: Mode -> [Int]
scaleSemisteps m = take (length ionianHs) . drop (fromEnum m) $ cycle ionianHs
  where ionianHs = [ 2 , 2 , 1 , 2 , 2 , 2 , 1 ]
