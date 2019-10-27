module Interval ( Interval (..)
                , SimpleInterval (..) -- for now
                , ratio
                , simpleRatio -- for now
                ) where

data SimpleInterval = MinorSecond
                    | MajorSecond
                    | MinorThird
                    | MajorThird
                    | PerfectFourth
                    | Tritone
                    | PerfectFifth
                    | MinorSixth
                    | MajorSixth
                    | MinorSeventh
                    | MajorSeventh
                    | Octave
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

data Interval = Prime
              | Compound Interval SimpleInterval
  deriving (Read, Eq, Ord)

instance Show Interval where
  show Prime = "Prime"
  show (Compound Prime si) = show si
  show (Compound i si) = show i ++ " + " ++ show si

simpleRatio :: SimpleInterval -> Rational
simpleRatio = undefined

ratio :: Interval -> Rational
ratio Prime                          = 1
ratio (Compound Prime MinorSecond)   = 16/15
ratio (Compound Prime MajorSecond)   = 9/8
ratio (Compound Prime MinorThird)    = 6/5
ratio (Compound Prime MajorThird)    = 5/4
ratio (Compound Prime PerfectFourth) = 4/3
ratio (Compound Prime Tritone)       = 45/32
ratio (Compound Prime PerfectFifth)  = 3/2
ratio (Compound Prime MinorSixth)    = 8/5
ratio (Compound Prime MajorSixth)    = 5/3
ratio (Compound Prime MinorSeventh)  = 7/4
ratio (Compound Prime MajorSeventh)  = 15/8
ratio (Compound Prime Octave)        = 2
ratio _                              = undefined
