module Note where

data Tone = C
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
  deriving (Read, Show, Eq, Ord, Enum)

data Note = Note OctaveCount Tone
  deriving (Read, Show, Eq, Ord)

instance Enum Note where
  fromEnum (Note octave tone) = 12 * fromEnum octave + fromEnum tone
  toEnum n = Note (toEnum octave) (toEnum tone)
    where (octave , tone) = n `divMod` 12

type OctaveCount = Integer
