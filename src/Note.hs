module Note ( Transposable (..)
            , NoteContainer (..)
            , ToneContainer (..)
            , Tone (..)
            , Note (..)
            ) where

import PrettyPrint

-- | A class for types that are based on the chromatic scale.
class Transposable a where
  -- | Transpose the given amount semisteps on the chromatic scale.
  transpose :: Int -- ^ semisteps
            -> a
            -> a

-- | Note on the chromatic scale.
data Note = C
          | CD
          | D
          | DE
          | E
          | F
          | FG
          | G
          | GA
          | A
          | AB
          | B
  deriving (Eq, Read, Show, Ord, Enum, Bounded)

instance PrettyPrint Note where
  pp C = "C"
  pp CD = "C♯"
  pp D = "D"
  pp DE = "D♯"
  pp E = "E"
  pp F = "F"
  pp FG = "F♯"
  pp G = "G"
  pp GA = "G♯"
  pp A = "A"
  pp AB = "A♯"
  pp B = "B"

instance Transposable Note where
  transpose i = toEnum . (`mod` (fromEnum (maxBound :: Note) + 1)) . (+ i) . fromEnum

-- | A class for data containing notes.
class NoteContainer c where
  mapNotes :: (Note -> Note) -> c -> c

instance NoteContainer Note where
  mapNotes = ($)

infix 5 :-
-- | Note on the chromatic scale with it's regarding octave.
data Tone = Note :- Int
  deriving (Eq, Read, Show)

instance PrettyPrint Tone where
  pp (note :- octave) = pp note ++ (toSubscript <$> show octave)
    where toSubscript = toEnum . (+ fromEnum '₀') . (+ (- fromEnum '0')) . fromEnum

instance Ord Tone where
  compare (n1 :- o1) (n2 :- o2) = if o1 == o2
                                  then compare n1 n2
                                  else compare o1 o2

instance Enum Tone where
  fromEnum (note :- octave) = 12 * fromEnum octave + fromEnum note
  toEnum n = toEnum note :- toEnum octave
    where (octave , note) = n `divMod` 12

instance Transposable Tone where
  transpose i = toEnum . (+ i) . fromEnum

class ToneContainer c where
  mapTones :: (Tone -> Tone) -> c -> c
