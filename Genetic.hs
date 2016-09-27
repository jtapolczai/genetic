module Genetic where

import Control.Monad (filterM)
import System.Random (randomRIO, Random)

import Debug.Trace

-- |A genetic algorithm with mutate, mate, select and fitness functions.
data GeneticAlgorithm elem fitness = GA {
   mutate :: elem -> IO elem,
   mate :: Combinator elem,
   select :: fitness -> fitness -> IO Bool,
   fitness :: elem -> fitness
}

-- |A recombinator function that generates child elements from parents.
type Combinator a = a -> a -> IO [a]

-- |A population of individuals
type Population a = [a]

-- |Runs a genetic algorithm for one time-step.
runGenetic
   :: Num fitness
   => GeneticAlgorithm elem fitness
   -> Population elem
   -> IO (Population elem)
runGenetic (GA mutate mate select fitness) pop = do
   let fitnesses = map (\x -> (x, fitness x)) pop
       sumFitness = sum . map snd $ fitnesses
   selectedPop <- filterM (\(_,fit) -> select sumFitness fit) fitnesses

   couples <- selectPairs $ map fst selectedPop
   children <- mapM (uncurry mate) couples
   mutatedChildren <- mapM mutate $ concat children
   return mutatedChildren

runSimulation :: Monad m => (a -> m a) -> Int -> a -> m a
runSimulation _ steps x | steps <= 0 = return x
runSimulation f steps x = do
   x' <- f x
   runSimulation f (steps - 1) x'

-- |Randomly and uniformly selects pairs from a list until it is
--  empty. If the list contains an odd number of elements, one element
--  will be discarded.
selectPairs :: [a] -> IO [(a,a)]
selectPairs [] = return []
selectPairs [_] = return []
selectPairs xs = do
   (x1, xs') <- selectRandomElem xs
   (x2, xs'') <- selectRandomElem xs'
   rest <- selectPairs xs''
   return $ (x1,x2) : rest

-- |Randomly and uniformly selects an element from a list.
--  The element and the list sans the selected element are returned.
--  If the list is empty, an error is raised.
selectRandomElem :: [a] -> IO (a,[a])
selectRandomElem [] = error "selectRandomElem: empty list"
selectRandomElem xs = do
   i <- randomRIO (0,length xs - 1)
   return (xs !! i, take i xs ++ drop (i+1) xs)

-- |Takes a value @x@ in the range [0,1] and returns True with probability @x@.
selectYesNo :: Float -> IO Bool
selectYesNo x = do
   y <- randomRIO (0,1)
   return (y < x)

-- |Selects @N@ items from a range.
randomRIOs
   :: Random a
   => (a,a)
   -> Int -- ^N
   -> IO [a] -- ^N-item list of random elements.
randomRIOs _ x | x <= 0 = return []
randomRIOs rng x = do
   this <- randomRIO rng
   rest <- randomRIOs rng (x-1)
   return (this:rest)

-- |Standard fitness selection function: myFitness / sumFitnesses.
stdSelect
   :: Float -- |Scaling factor for the fitness.
   -> Float -- Constant factor for the fitness
   -> Float -> Float -> IO Bool
stdSelect a b sumFitness myFitness = selectYesNo ((myFitness / sumFitness) * a + b)

findNumber
   :: Int -- ^The target number.
   -> Int -- ^Mutation rate.
   -> GeneticAlgorithm Int Float
findNumber target mutateRate = GA mutate mate (stdSelect 10 0 ) fitness
   where
      mutate x = do
         y <- randomRIO (negate mutateRate, mutateRate)
         return (x+y)

      mate x y = return [round $ a + ((b-a) * (1/3)),
                         round $ a + ((b-a) * (2/3))]
         where
            (a,b) = if x <= y then (fromIntegral x, fromIntegral y)
                              else (fromIntegral y,fromIntegral x)

      fitness x = max 0 (1 - (diff / fromIntegral target))
         where
            diff :: Float
            diff = fromIntegral $ abs (x - target)

numberSimulation :: Int -> IO (Population Int)
numberSimulation steps = runSimulation (runGenetic (findNumber 100 10)) steps [1..200]

-- |Datatypes that can be represented as bits.
class BitRepresentation a where
   toBits :: a -> [Bool]
   fromBits :: [Bool] -> a
   bitLength :: a -> Int

instance BitRepresentation Int where
   toBits x = padLeft False (bitLength (0 :: Int)) (go x)
      where go x | x == 1    = [True]
                 | x == 0    = [False]
                 | otherwise = go (x `div` 2) ++ [(x `mod` 2 == 1)]

   fromBits x = sum $ zipWith mult (reverse x) powers
      where
         mult True x = x
         mult False _ = 0
         powers = map (2^) [0..]

   bitLength _ = 16

-- |Pads a list to the left to a maximum length of @len@.
padLeft
   :: Show a => a -- ^Padding element.
   -> Int -- ^The resultant length.
   -> [a]
   -> [a]
padLeft elem len xs = replicate (len - length xs) elem ++ xs

-- |Takes two parents introduces a single point of crossover in their
--  bit representations.
crossover :: BitRepresentation a => Combinator a
crossover x y = do
   let xb = toBits x
       yb = toBits y
   cp <- randomRIO (0, length xb)
   let xChild = take cp xb ++ drop cp yb
       yChild = take cp yb ++ drop cp xb
   return [fromBits xChild, fromBits yChild]

-- |Takes two parents introduces a two points of crossover in their
--  bit representations.
twoPointCrossover :: BitRepresentation a => Combinator a
twoPointCrossover x y = do
   let xb = toBits x
       yb = toBits y
   cp1 <- randomRIO (0, length xb)
   cp2 <- randomRIO (0, length xb)
   let (a,b) = if cp1 <= cp2 then (cp1,cp2) else (cp2,cp1)
       diff = b - a
   let xChild = take a xb ++ (take diff . drop a $ yb) ++ drop b xb
       yChild = take a yb ++ (take diff . drop a $ xb) ++ drop b yb
   return [fromBits xChild, fromBits yChild]

-- |Shows a sequence of bits as a series of 1s and 0s.
bitshow :: [Bool] -> String
bitshow = map f
   where
      f False = '0'
      f True = '1'
