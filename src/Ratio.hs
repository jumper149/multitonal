module Ratio where

import Interval

fromInterval :: Interval -> Rational
fromInterval Prime         = 1
fromInterval MinorSecond   = 16/15
fromInterval MajorSecond   = 9/8
fromInterval MinorThird    = 6/5 
fromInterval MajorThird    = 5/4
fromInterval PerfectFourth = 4/3
fromInterval Tritone       = 45/32
fromInterval PerfectFifth  = 3/2
fromInterval MinorSixth    = 8/5
fromInterval MajorSixth    = 5/3
fromInterval MinorSeventh  = 7/4
fromInterval MajorSeventh  = 15/8
fromInterval Octave        = 2
