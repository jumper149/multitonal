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
import PrettyPrint

import qualified Data.List.NonEmpty as NE

-- | Notes from the diatonic 'Scale' stacked on top of each other.
newtype Chord = Chord (NE.NonEmpty Note)
  deriving (Eq, Read, Show)

instance PrettyPrint Chord where
  pp (Chord ne) = "Chord " ++ unwords (pp <$> NE.toList ne)

instance NoteContainer Chord where
  mapNotes f (Chord ne) = Chord $ f <$> ne

instance Transposable Chord where
  transpose n = mapNotes $ transpose n

-- | The relation of a 'Chord' to the tonal centre of a 'Scale'.
data Function = Tonic
              | Supertonic
              | Mediant
              | Subdominant
              | Dominant
              | Submediant
              | Leading
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance PrettyPrint Function where
  pp = show

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
prettyShowChord c = either ((show root ++) . show) ((show root ++) . show) $ chordType c
  where Chord (root NE.:| _) = c

data ChordType = MajorTriad
               | AugmentedTriad -- not from Scale
               | MinorTriad
               | DiminishedTriad

               | MajorSeventh
               | DominantSeventh
               | MinorSeventh
               | HalfdiminishedSeventh

               | MajorNinth
               | DominantmajorNinth
               | MinorNinth
               | DominantminorNinth
               | DiminisheddominantminorNinth
  deriving (Eq, Read, Show)

instance PrettyPrint ChordType where
  pp MajorTriad = "maj"
  pp AugmentedTriad = "aug"
  pp MinorTriad = "min"
  pp DiminishedTriad = "dim"

  pp MajorSeventh = "maj⁷"
  pp DominantSeventh = "dom⁷"
  pp MinorSeventh = "min⁷"
  pp HalfdiminishedSeventh = "min⁷♭5"

  pp MajorNinth = "maj⁹"
  pp DominantmajorNinth = "dom⁹"
  pp MinorNinth = "min⁹"
  pp DominantminorNinth = "dom⁷♭9"
  pp DiminisheddominantminorNinth =  "dom⁷♭5♭9"

chordType :: Chord -> Either ChordType [Int]
chordType (Chord (root NE.:| rest))
  | steps == [ 4 , 7 ] = Left MajorTriad
  | steps == [ 4 , 8 ] = Left AugmentedTriad
  | steps == [ 3 , 7 ] = Left MinorTriad
  | steps == [ 3 , 6 ] = Left DiminishedTriad

  | steps == [ 4 , 7 , 11 ] = Left MajorSeventh
  | steps == [ 4 , 7 , 10 ] = Left DominantSeventh
  | steps == [ 3 , 7 , 10 ] = Left MinorSeventh
  | steps == [ 3 , 6 , 10 ] = Left HalfdiminishedSeventh

  | steps == [ 4 , 7 , 11 , 2 ] = Left MajorNinth
  | steps == [ 4 , 7 , 10 , 2 ] = Left DominantmajorNinth
  | steps == [ 3 , 7 , 10 , 2 ] = Left MinorNinth
  | steps == [ 3 , 7 , 10 , 1 ] = Left DominantminorNinth
  | steps == [ 3 , 6 , 10 , 1 ] = Left DiminisheddominantminorNinth

  | otherwise = Right steps
  where steps = (`mod` (fromEnum (maxBound :: Note) + 1)) . (+ (- fromEnum root)) . fromEnum <$> rest
