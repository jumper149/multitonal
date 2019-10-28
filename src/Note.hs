module Note ( Transposable (..)
            , Tone (..)
            , Note (..)
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
