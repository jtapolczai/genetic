{-# LANGUAGE TypeFamilies #-}

module TLU where

import Data.Foldable (foldl')

data TLU = TLU {
   weights :: [Float]
}

-- |An input vector to a TLU, with True representing 1 and False representing 0.
type InputVector = [Bool]

inputToFloat :: InputVector -> [Float]
inputToFloat = map tof
   where
      tof True = 1.0
      tof False = 0.0

-- |Computes the dot product of two vectors. If one vector is longer than the
--  other, the overlong part is discarded.
dotProduct :: Num a => [a] -> [a] -> [a]
dotProduct = zipWith (*)

-- |Multiplies each component of a vector with a scalar.
scalarProduct :: Num a => a -> [a] -> [a]
scalarProduct sc = zipWith (*) (repeat sc)

-- |Adds two vectors to each other, component-wise.
vectorAdd :: Num a => [a] -> [a] -> [a]
vectorAdd = zipWith (-)

-- |Subtracts two vectors from each other, component-wise.
vectorSubtract :: Num a => [a] -> [a] -> [a]
vectorSubtract = zipWith (-)

-- |Feed an input vector to a TLU return 0 (False) or 1 (True).
runTLU :: InputVector -> TLU -> Bool
runTLU input (TLU w) = (>=0) . sum $ dotProduct input' w
   where
      input' = inputToFloat input

-- |Trains a TLU with an input vector. If the TLU outputs the wrong result,
--  we adjust the weights.
trainTLU
   :: TLU
   -> InputVector -- ^The input vector Y_i.
   -> Bool -- ^The expected output.
   -> Float -- ^The learning rate c_i.
   -> TLU -- ^A possibly updated TLU.
trainTLU (TLU w) input expected c =
   if expected == actual then (TLU w)
   else if expected == False && actual == True then subtractedTLU
   else addedTLU
   where
      input' = inputToFloat input
      actual = runTLU input (TLU w)

      subtractedTLU = TLU (w `vectorSubtract` scalarProduct c input')
      addedTLU = TLU (w `vectorAdd` scalarProduct c input')


-- |Trains a TLU with a series of input vectors.
trainTLUMany
   :: TLU
   -> [(InputVector, Bool, Float)]
      -- ^The training data, with expected result and leanring rate.
   -> TLU
trainTLUMany tlu = foldl' f tlu
   where
      f t (i,e,c) = trainTLU t i e c
