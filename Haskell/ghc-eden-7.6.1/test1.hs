-------------------------------------------------------------
-- Generic scheme to deal with Particle Swarm Optimization --
--                                                         --
-- This file contains examples of fitness functions, as    --
-- well as examples of main functions to call to the       --
-- generic PSO schemes defined in module PSO.              -- 
--                                                         --
-- Created by: P. Rabanal, I. Rodriguez, F. Rubio          --
-- Last modified: June 2012                                --
-------------------------------------------------------------

module Main(main) where
import PSO
import Data.List
import System.Random
import System.Environment(getArgs)


-------------------------------------------
-- Auxiliary main functions calling to   --
-- different sequential/parallel schemes --
-------------------------------------------

mainSeq :: WPGparams -> Int -> Int -> (Position -> Double) -> Boundings -> IO()
mainSeq wpg np nit f bo
  = do sg <- getStdGen
       let bestPos = psoSEQ sg wpg np nit f bo
       putStr "Best value: " 
       print (fst bestPos)
       putStr "Best position: "
       print (snd bestPos)

{--
mainPar :: WPGparams -> Int -> Int -> Int -> Int -> (Position -> Double) -> Boundings -> IO()
mainPar wpg np npit nit nPE f bo
  = do sg <- getStdGen
       let bestPos = pso sg wpg np npit nit nPE f bo
       print (fst bestPos)

mainParV :: WPGparams -> Int -> Int -> Int -> [Double] -> (Position -> Double) -> Boundings -> IO()
mainParV wpg np npit nit speeds f bo
  = do sg <- getStdGen
       let bestPos = psoVar sg wpg np npit nit speeds f bo
       print (fst bestPos)

mainParV2 :: WPGparams -> Int -> [Int] -> [Double] -> (Position -> Double) -> Boundings -> IO()
mainParV2 wpg np npits speeds f bo
  = do sg <- getStdGen
       let bestPos = psoVar2 sg wpg np npits speeds f bo
       print (fst bestPos)
--}

-------------------
-- Main Function --
-------------------

-- Main function that can call to different sequential or parallel schemes
-- Examples of use (assumming the executable to be a.out):
--           
--    ./a.out 3 1 100 5000 +RTS -N1
--           (problem number 3) (version 1: sequential) (100 particles) (5000 iterations) (1 processor)
--    ./a.out 1 2 100 100 15 2 +RTS -N2
--           (problem number 1) (version 2: basic parallel scheme) (100 particles) 
--           (100 iterations per parallel step) (15 parallel steps) (2 islands) (2 processors)
--    ./a.out 1 3 100 100 15 20 30 37 +RTS -N3
--           (problem number 1) (version 3: parallel, processors with different speeds)
--           (100 particles) (100 iterations per parallel step) (15 parallel steps) 
--           (3 processors with relative speeds 20, 30, and 37)
--

mainInterpreter prob np nit = mainSeq wpg1 np nit (fit prob) (bo prob)

main = do args <- getArgs
          let args' :: [Int] 
              (problem:version:np:args') = map read args
              (npit:nit:rest) = args'
              (npe:speedsINT,iterations) = splitAt (npit+1) args' 
              speeds :: [Double]
              speeds = map fromIntegral speedsINT
              iterations' = iterationsModel (head (iterations))
          putStr "Sol: "
          case version  of
             1 -> mainSeq wpg1 np (head args') (fit problem) (bo problem) 
{--
             2 -> mainPar wpg1 np npit nit (head rest) (fit problem) (bo problem)
             3 -> mainParV wpg1 np npit nit (map fromIntegral rest) (fit problem) (bo problem)
             4 -> mainParV2 wpg1 np iterations speeds (fit problem) (bo problem)
             5 -> mainParV2 wpg1 np iterations' speeds (fit problem) (bo problem)
--}

-------------------------------
-- CONCRETE RUNNING EXAMPLES --
-------------------------------

np1 :: Int
np1 = 20

nit1 :: Int
nit1 = 100

--bo1 :: Boundings

--fit1 :: (Position -> Double)

mainTest = mainSeq wpg1 np1 nit1 (fit 1) (bo 1)


-- Example of adjustment parameters 
-- (taken from M.E.H Pedersen, Tuning & Simplifying Heuristical Optimization)
wpg1 :: (Double,Double,Double)
wpg1 = (-0.16,1.89,2.12)
wpg2 :: (Double,Double,Double)
wpg2 = (0.7,1.45,1.49)


-- Examples of fitness functions with corresponding boundings. taken from Yao et al 
-- (Evolutionary Programming made faster, IEEE Trans. on Evolutionary Computation)
bo :: Int -> Boundings
bo 1 = replicate 30 (-100,100)
bo 2 = replicate 30 (-10,10)

fit :: Int -> (Position -> Double)
fit 1 xs = sum (map sqr xs)
fit 2 xs = sum xs' + (foldr (*) 1 xs')
  where xs' = map abs xs

-- Examples of varying number of iterations in each parallel step
iterationsModel :: Int -> [Int]
iterationsModel 1 = replicate 20 50 ++ replicate 40 200


-- Auxiliary functions
sqr x = x*x
prod xs = foldr (*) 1 xs


--Experiment stuff
--bo1 = replicate 30 (-100,100)
--fit1 xs = sum (map sqr xs)

--bo2 :: Boundings
--bo2 = replicate 30 (-10,10)
--fit2 :: (Position -> Double)
--fit2 xs = sum xs' + (foldr (*) 1 xs')
--  where xs' = map abs xs

{------------------------------------
------------------------------------
     Finance functions
------------------------------------     
------------------------------------}

--Risky characteristic
risk = 0.5
--Risk aversion
aversion = 3
--Required expected return
reqExpR = 0.02

--Number of assets
nAssets :: Int
nAssets = 5

--Set of assets 
assets :: [Int]
assets = [1,2..nAssets]

--Weights of each asset
weights :: Position
--weights = replicate nAssets 0.2
weights = [0.1,0.3,0.15,0.25,0.2]

----Return of each asset
--rateR :: Position
--rateR = [0.09,0.03, 0.7, 0.1,0.01]

----Expected return of portfolio
--expR :: Position
--expR = [0.01,0.05, 0.4, 0.2,0.01]

--Return of each asset
rateR :: Position
rateR = [0.0091,0.0090, 0.0107, 0.0041,0.0118]

--Expected return of portfolio
expR :: Position
expR = [0.0091,0.0090, 0.0107, 0.0041,0.0118]

--Return of portfolio
portR :: Position -> Double
portR w = sum [x*y | x <- w, y <- rateR]

--Expected Portfolio    return 
expPortR :: Position -> Double
expPortR w = sum [x*y | x <- w, y <- expR]

--Portfolio function 
port :: Position -> Double
port w = risk*(max 0 ((portR w)-(expPortR w)))+(1-risk)*((max 0 ((expPortR w)-(portR w)))**(1/aversion))-(expPortR w)

--Penalty parameter-
penPara = 0.1
--Penalty value
penVal = 1/penPara

--Unconstrained porfolio function 
mainPortFunction :: Position -> Double
mainPortFunction w = (port w) + penVal*(abs((expPortR w)-reqExpR)) + penVal*(abs((sum w) - 1))

weightsBound :: Boundings
weightsBound = replicate nAssets (0,1)

portSeq :: WPGparams -> Int -> Int -> (Position -> Double) -> Boundings -> IO()
portSeq wpg np nit f bo
  = do sg <- getStdGen
       let bestPos = psoSEQ sg wpg np nit f bo
       putStr "Best value: " 
       print (fst bestPos)
       putStr "Best position: "
       print (snd bestPos)
       putStr "Sum of weights: "
       print (sum (snd bestPos))
       putStr "Expected Portfolio Return: "
       print (expPortR (snd bestPos))

-- mainPortText number of particles, iterations problem and bounds
mainPortTest np nit prob bound = portSeq wpg1 np nit prob bound

--Showing example of function
mainPortTest1 = mainPortTest 20 100 mainPortFunction weightsBound