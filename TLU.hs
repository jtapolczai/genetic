{-# LANGUAGE TypeFamilies #-}

module TLU where

import Data.Foldable (foldl')
import Data.Ord (comparing)
import qualified Data.Vector as V

-- |A 2-category treshold linear unit.
data TLU = TLU {
   weights :: [Float]
}

-- |An R-category linear meachine.
data LinearMachine = LM {
   lmweights :: V.Vector [Float]
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
dotProduct :: Num a => [a] -> [a] -> a
dotProduct x y = sum $ zipWith (*) x y

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
runTLU input (TLU w) = (>=0) $ dotProduct input' w
   where
      input' = inputToFloat input

-- |Feed an input vector to a linear machine return a classification from 0
--  to N.
runLinearMachine :: InputVector -> LinearMachine -> Int
runLinearMachine input (LM ws) =
   fst
   . V.maximumBy (comparing snd)
   . fmap (\(i, w) -> (i, dotProduct input' w))
   . V.zip oneToN
   $ ws
   where
      oneToN = V.iterateN (V.length ws) (+1) 1
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
      -- ^The training data, with expected result and learning rate.
   -> TLU
trainTLUMany tlu = foldl' f tlu
   where
      f t (i,e,c) = trainTLU t i e c

trainTLUFractional
   :: TLU
   -> InputVector
   -> Bool
   -> Float -- ^Lambda
   -> TLU
trainTLUFractional (TLU w) input expected lambda = trainTLU (TLU w) input expected c
   where
      input' = inputToFloat input
      c = lambda * ((dotProduct input' w) / (dotProduct input' input'))

-- |Trains a TLU with a series of input vectors.
trainTLUManyFractional
   :: TLU
   -> Float -- ^Lambda.
   -> [(InputVector, Bool)]
      -- ^The training data, with expected result.
   -> TLU
trainTLUManyFractional tlu lambda = foldl' f tlu
   where
      f t (i,e) = trainTLUFractional t i e lambda
