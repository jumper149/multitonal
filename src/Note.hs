module Note ( Tone (..)
            , Note (..)
            , OctaveCount -- hide?
            ) where

-- | Tone on the chromatic scale.
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
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

-- | Tone on the chromatic scale with it's regarding octave.
data Note = Note OctaveCount Tone
  deriving (Read, Eq, Ord)

instance Show Note where
  show (Note octave tone) = show tone ++ (toSubscript <$> show octave)
    where toSubscript = toEnum . (+ fromEnum '₀') . (+ (- fromEnum '0')) . fromEnum

instance Enum Note where
  fromEnum (Note octave tone) = 12 * fromEnum octave + fromEnum tone
  toEnum n = Note (toEnum octave) (toEnum tone)
    where (octave , tone) = n `divMod` 12

type OctaveCount = Integer
