module Chord ( Chord (..)
             , Function (..)
             , triad
             , seventh
             , ninth
             , stackThirds
             , prettyShowChord
             ) where

import Note
import Diatonic

import qualified Data.List.NonEmpty as NE

-- | Notes from the diatonic 'Scale' stacked on top of each other.
newtype Chord = Chord (NE.NonEmpty Note)
  deriving (Read, Eq)

instance Show Chord where
  show (Chord ne) = "Chord " ++ unwords (show <$> NE.toList ne)

instance Transposable Chord where
  transpose n (Chord ne) = Chord $ transpose n <$> ne

-- | The relation of a 'Chord' to the tonal centre of a 'Scale'.
data Function = Tonic
              | Supertonic
              | Mediant
              | Subdominant
              | Dominant
              | Submediant
              | Leading
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

triad :: Mode -> Function -> Note -> Chord
triad = stackThirds 3

seventh :: Mode -> Function -> Note -> Chord
seventh = stackThirds 4

ninth :: Mode -> Function -> Note -> Chord
ninth = stackThirds 5

stackThirds :: Int -> Mode -> Function -> Note -> Chord
stackThirds i m f n
  | i > 0 = Chord $ (NE.fromList . take i . function) f <*> pure (scale m n)
  | otherwise = undefined

-- | Return an infinite list of functions that access stacked thirds from a 'Scale'.
function :: Function -> [Scale -> Note]
function f = drop (4 * fromEnum f) accessors
  where accessors = cycle [ s1 , s3 , s5 , s7 , s2 , s4 , s6 ]

prettyShowChord :: Chord -> String
prettyShowChord c = maybe (show c) ((show root ++) . show) $ chordType c
  where Chord (root NE.:| _) = c

data ChordType = MajorTriad
               | MinorTriad
               | MajorSeventh
               | MinorSeventh
               | DominantSeventh
  deriving (Read, Eq)

instance Show ChordType where
  show MajorTriad = "maj"
  show MinorTriad = "min"
  show MajorSeventh = "maj⁷"
  show MinorSeventh = "min⁷"
  show DominantSeventh = "dom⁷"

chordType :: Chord -> Maybe ChordType
chordType (Chord (root NE.:| rest))
  | steps == [ 4 , 7 ] = Just MajorTriad
  | steps == [ 3 , 7 ] = Just MinorTriad
  | steps == [ 4 , 7 , 11 ] = Just MajorSeventh
  | steps == [ 3 , 7 , 10 ] = Just MinorSeventh
  | steps == [ 4 , 7 , 10 ] = Just DominantSeventh
  | otherwise = Nothing
  where steps = (`mod` (fromEnum (maxBound :: Note) + 1)) . (+ (- fromEnum root)) . fromEnum <$> rest
