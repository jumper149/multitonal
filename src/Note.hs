module Note where

data Tone = C
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
  deriving (Read, Show, Eq, Ord, Enum)

data Note = Note OctaveCount Tone
  deriving (Read, Show, Eq, Ord)

type OctaveCount = Integer
