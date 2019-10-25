module Note ( Tone (..)
            , Note (..)
            , OctaveCount -- hide?
            ) where

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
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

-- | Note on the chromatic scale with it's regarding octave.
data Tone = Tone OctaveCount Note
  deriving (Read, Eq, Ord)

instance Show Tone where
  show (Tone octave tone) = show tone ++ (toSubscript <$> show octave)
    where toSubscript = toEnum . (+ fromEnum '₀') . (+ (- fromEnum '0')) . fromEnum

instance Enum Tone where
  fromEnum (Tone octave tone) = 12 * fromEnum octave + fromEnum tone
  toEnum n = Tone (toEnum octave) (toEnum tone)
    where (octave , tone) = n `divMod` 12

type OctaveCount = Integer
