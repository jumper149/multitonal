module Polytone ( fromChord
                , polyMap
                , polyToList
                , polyFromList
                ) where

import Note
import Chord

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

newtype Polytone = Polytone (S.Set Tone)
  deriving (Read, Eq)

instance Show Polytone where
  show (Polytone s) = "Polytone " ++ unwords (show <$> S.toList s)

fromChord :: Int      -- ^ octave count
          -> Chord
          -> Polytone
fromChord n c = Polytone . S.fromList $ ascendingTone rootTone rest
  where rootTone = root :- n
        Chord (root NE.:| rest) = c

ascendingTone :: Tone -> [Note] -> [Tone]
ascendingTone prev []     = [ prev ]
ascendingTone prev (n:ns) = prev : ascendingTone next ns
  where next = if prev < lower
               then lower
               else higher
        higher = n :- i + 1
        lower = n :- i
        _ :- i = prev

polyMap :: (Tone -> Tone) -> Polytone -> Polytone
polyMap f (Polytone s) = Polytone . S.map f $ s

polyFromList :: [Tone] -> Polytone
polyFromList = Polytone . S.fromList

polyToList :: Polytone -> [Tone]
polyToList (Polytone s) = S.toList s
