module Polytone ( Polytone
                , polytoneFromChord
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
import PrettyPrint

import qualified Data.Set as S

newtype Polytone = Polytone (S.Set Tone)
  deriving (Eq, Read, Show)

instance Semigroup Polytone where
  (<>) = polytoneUnion

instance Monoid Polytone where
  mempty = polytoneEmpty

instance PrettyPrint Polytone where
  pp p = "{" ++ unwords (show <$> polytoneToList p) ++ "}"

instance ToneContainer Polytone where
  mapTones f (Polytone s) = Polytone $ S.map f s

-- | Create a 'Polytone' from a 'Chord'.
polytoneFromChord :: Int      -- ^ octave count
                  -> Chord
                  -> Polytone
polytoneFromChord n c = Polytone . S.fromList $ ascendingTone rootTone rest
  where rootTone = root :- n
        root:rest = listNotes c

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
polytoneToList (Polytone s) = S.toAscList s

polytoneInsert :: Tone -> Polytone -> Polytone
polytoneInsert t (Polytone s) = Polytone . S.insert t $ s

polytoneDelete :: Tone -> Polytone -> Polytone
polytoneDelete t (Polytone s) = Polytone . S.delete t $ s

polytoneMap :: (Tone -> Tone) -> Polytone -> Polytone
polytoneMap f (Polytone s) = Polytone . S.map f $ s

polytoneUnion :: Polytone -> Polytone -> Polytone
polytoneUnion (Polytone s) (Polytone t) = Polytone $ S.union s t
