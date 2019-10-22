module Note where

data Tone = C
          | Cs
          | Db
          | D
          | Ds
          | Eb
          | E
          | F
          | Fs
          | Gb
          | G
          | Gs
          | Ab
          | A
          | As
          | Bb
          | B
  deriving (Read, Show)

instance Eq Tone where
  (==) Cs Db = True
  (==) Ds Eb = True
  (==) Fs Gb = True
  (==) Gs Ab = True
  (==) As Bb = True
  (==) _  _  = False

data Note = Note Tone OctaveCount
  deriving (Read, Show, Eq)

type OctaveCount = Integer
