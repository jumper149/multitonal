module Chord ( Chord
             , toNonEmpty
             , Function (..)
             , triad
             , seventh
             , ninth
             , stackThirds
             , showChord -- improve later
             ) where

import Note
import Diatonic
import Interval

import qualified Data.List.NonEmpty as NE
import Data.Char ( toLower
                 , toUpper
                 )

newtype Chord = Chord (NE.NonEmpty Note)
  deriving (Read, Eq)

instance Show Chord where
  show (Chord ne) = "Chord " ++ unwords (show <$> NE.toList ne)

showChord :: Chord -> String
showChord (Chord (root NE.:| rest)) = if weird
                                      then show (Chord (root NE.:| rest))
                                      else base ++ seven
  where minor = if transpose (fromEnum MinorThird) root `elem` rest
                then fmap toLower
                else fmap toUpper
        base = minor $ show root
        seven
          | transpose (fromEnum MinorSeventh) root `elem` rest = "7"
          | transpose (fromEnum MajorSeventh) root `elem` rest = "maj7"
          | otherwise = ""
        weird = notElem (transpose (fromEnum PerfectFifth) root) rest
             || (notElem (transpose (fromEnum MinorThird) root) rest && notElem (transpose (fromEnum MajorThird) root) rest)
             || length rest > 3

instance Transposable Chord where
  transpose n (Chord ne) = Chord $ transpose n <$> ne

toNonEmpty :: Chord -> NE.NonEmpty Note
toNonEmpty (Chord ne) = ne

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
