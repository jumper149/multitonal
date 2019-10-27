module Polytone ( fromChord
                , polytoneEmpty
                , polytoneSingleton
                , polytoneToList
                , polytoneFromList
                , polytoneInsert
                , polytoneDelete
                , polytoneMap
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

polytoneEmpty :: Polytone
polytoneEmpty = Polytone S.empty

polytoneSingleton :: Tone -> Polytone
polytoneSingleton = Polytone . S.singleton

polytoneFromList :: [Tone] -> Polytone
polytoneFromList = Polytone . S.fromList

polytoneToList :: Polytone -> [Tone]
polytoneToList (Polytone s) = S.toList s

polytoneInsert :: Tone -> Polytone -> Polytone
polytoneInsert t (Polytone s) = Polytone . S.insert t $ s

polytoneDelete :: Tone -> Polytone -> Polytone
polytoneDelete t (Polytone s) = Polytone . S.delete t $ s

polytoneMap :: (Tone -> Tone) -> Polytone -> Polytone
polytoneMap f (Polytone s) = Polytone . S.map f $ s
