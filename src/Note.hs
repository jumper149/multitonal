module Note ( Transposable (..)
            , Tone (..)
            , Note (..)
            ) where

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
  deriving (Read, Eq, Ord, Enum, Bounded)

instance Show Note where
  show C = "C"
  show CD = "C♯"
  show D = "D"
  show DE = "D♯"
  show E = "E"
  show F = "F"
  show FG = "F♯"
  show G = "G"
  show GA = "G♯"
  show A = "A"
  show AB = "A♯"
  show B = "B"

instance Transposable Note where
  transpose i = toEnum . (`mod` (fromEnum (maxBound :: Note) + 1)) . (+ i) . fromEnum

infix 5 :-
-- | Note on the chromatic scale with it's regarding octave.
data Tone = Note :- Int
  deriving (Read, Eq)

instance Show Tone where
  show (tone :- octave) = show tone ++ (toSubscript <$> show octave)
    where toSubscript = toEnum . (+ fromEnum '₀') . (+ (- fromEnum '0')) . fromEnum

instance Ord Tone where
  compare (t1 :- o1) (t2 :- o2) = if o1 == o2
                                  then compare t1 t2
                                  else compare o1 o2

instance Enum Tone where
  fromEnum (tone :- octave) = 12 * fromEnum octave + fromEnum tone
  toEnum n = toEnum tone :- toEnum octave
    where (octave , tone) = n `divMod` 12

instance Transposable Tone where
  transpose i = toEnum . (+ i) . fromEnum
