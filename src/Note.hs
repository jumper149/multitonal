module Note ( Transposable (..)
            , Tone (..)
            , Note (..)
            , OctaveCount -- hide?
            ) where

class Transposable a where
  transpose :: Int -> a -> a


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

instance Transposable Note where
  transpose i = toEnum . (`mod` (fromEnum (maxBound :: Note) + 1)) . (+ i) . fromEnum

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

instance Transposable Tone where
  transpose i = toEnum . (+ i) . fromEnum

type OctaveCount = Integer
