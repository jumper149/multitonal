module Interval ( Interval (..)
                , SimpleInterval (..) -- for now
                , ratio
                , interval
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

-- from https://en.wikipedia.org/wiki/Interval_(music)#Size_of_intervals_used_in_different_tuning_systems
simpleRatio :: SimpleInterval -> Rational
simpleRatio MinorSecond   = 16/15
simpleRatio MajorSecond   = 9/8
simpleRatio MinorThird    = 6/5
simpleRatio MajorThird    = 5/4
simpleRatio PerfectFourth = 4/3
simpleRatio Tritone       = 45/32 -- or 25:18 ?
simpleRatio PerfectFifth  = 3/2
simpleRatio MinorSixth    = 8/5
simpleRatio MajorSixth    = 5/3
simpleRatio MinorSeventh  = 16/9
simpleRatio MajorSeventh  = 15/8
simpleRatio Octave        = 2

infixl 5 :+:
infixl 5 :-:
data Interval = Prime
              | Interval :+: SimpleInterval
              | Interval :-: SimpleInterval
  deriving (Read, Eq, Ord)

instance Show Interval where
  show Prime = "Prime"
  show (Prime :+: si) = show si
  show (Prime :-: si) = '-' : show si
  show (i :+: si) = show i ++ "+" ++ show si
  show (i :-: si) = show i ++ "-" ++ show si

ratio :: Interval -> Rational
ratio Prime = 1
ratio (i :+: si)   = ratio i * simpleRatio si
ratio (i :-: si)   = ratio i / simpleRatio si

-- | Returns the 'Interval' corresponding to the given amount of semisteps.
interval :: Int      -- ^ semisteps
         -> Interval
interval n
  | abs n <= fromEnum (maxBound :: SimpleInterval) =
    if n == 0
    then Prime
    else if n > 0
         then Prime :+: toEnum (n - 1)
         else Prime :+: toEnum (-n + 1)
  | otherwise = undefined
