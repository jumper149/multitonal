module Ratio where

import Interval

fromInterval :: Interval -> Rational
fromInterval (Compound n Prime)         = 2^n * 1
fromInterval (Compound n MinorSecond)   = 2^n * 16/15
fromInterval (Compound n MajorSecond)   = 2^n * 9/8
fromInterval (Compound n MinorThird)    = 2^n * 6/5
fromInterval (Compound n MajorThird)    = 2^n * 5/4
fromInterval (Compound n PerfectFourth) = 2^n * 4/3
fromInterval (Compound n Tritone)       = 2^n * 45/32
fromInterval (Compound n PerfectFifth)  = 2^n * 3/2
fromInterval (Compound n MinorSixth)    = 2^n * 8/5
fromInterval (Compound n MajorSixth)    = 2^n * 5/3
fromInterval (Compound n MinorSeventh)  = 2^n * 7/4
fromInterval (Compound n MajorSeventh)  = 2^n * 15/8
fromInterval (Compound n Octave)        = 2^n * 2
