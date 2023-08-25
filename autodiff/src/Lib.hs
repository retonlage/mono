{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Lib
    () where

-- example function (implementation a single layer with relu)
linear :: [[Par]] -> [Par] -> [Par] -> [Par]
linear wss bs xs = map (\(ws, b) -> relu $ sum $ b : zipWith (*) ws xs) (zip wss bs)
  where
    relu x = Lib.max (Constant 0) x

data Par =
  Constant Float |
  Add Par Par | Sub Par Par |
  Mul Par Par | Div Par Par |
  Pow Par Par | Log Par |
  Max Par Par |
  Eq  Par Par |
  Sin Par | Cos Par | Tan Par 
  deriving (Show)

instance Num Par where
  (+) = Add
  (-) = Sub
  (*) = Mul
  abs = undefined
  signum = undefined
  fromInteger = Constant . fromInteger

max :: Par -> Par -> Par
max = Max

interp :: Par -> Float
interp (Constant x) = x
interp (Add x y) = interp x + interp y
interp (Sub x y) = interp x - interp y
interp (Mul x y) = interp x * interp y
interp (Div x y) = interp x / interp y
interp (Pow x y) = interp x ** interp y
interp (Max x y) = Prelude.max (interp x) (interp y)
interp (Eq  x y) = if interp x == interp y then 1 else 0
interp (Sin x) = Prelude.sin $ interp x
interp (Cos x) = Prelude.cos $ interp x
interp (Tan x) = Prelude.tan $ interp x
interp (Log x) = Prelude.log $ interp x
