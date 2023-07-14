module Lib
    () where

data Calc = Add Calc Calc| Sub Calc Calc | Mul Calc Calc | Div Calc Calc
  | Constant Float
data Dual = Dual Float Calc

instance Num Dual where
  (+) x1 x2 = Dual (r1 + r2)
    where
      Dual r1 d1 = x1
      Dual r2 d2 = x2
