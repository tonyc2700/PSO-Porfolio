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
       print (fst bestPos)

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
             2 -> mainPar wpg1 np npit nit (head rest) (fit problem) (bo problem)
             3 -> mainParV wpg1 np npit nit (map fromIntegral rest) (fit problem) (bo problem)
             4 -> mainParV2 wpg1 np iterations speeds (fit problem) (bo problem)
             5 -> mainParV2 wpg1 np iterations' speeds (fit problem) (bo problem)


-------------------------------
-- CONCRETE RUNNING EXAMPLES --
-------------------------------

-- Example of adjustment parameters 
-- (taken from M.E.H Pedersen, Tuning & Simplifying Heuristical Optimization)
wpg1 :: (Double,Double,Double)
wpg1 = (-0.16,1.89,2.12)

--wpg2 = (-0.9,2,2)


-- Examples of fitness functions with corresponding boundings. taken from Yao et al 
-- (Evolutionary Programming made faster, IEEE Trans. on Evolutionary Computation)
bo :: Int -> Boundings
bo 1 = replicate 30 (-5.21,5.12)
bo 2 = replicate 30 (-10,10)
bo 3 = replicate 30 (-100,100)
bo 4 = replicate 30 (-100,100)
bo 5 = replicate 30 (-30,30)
bo 6 = replicate 30 (-100,100)
bo 8 = replicate 30 (-500,500)
bo 9 = replicate 30 (-5.12,5.12)
bo 10 = replicate 30 (-32,32)
bo 11 = replicate 30 (-600,600)
bo 12 = replicate 30 (-50,50)
bo 13 = replicate 30 (-50,50)
bo 14 = replicate 2 (-65536,65536)
bo 15 = replicate 4 (-5,5)
bo 16 = [(-5,5),(-5,5)]
bo 17 = [(-5,10),(0,15)]
bo _ = bo 17

fit :: Int -> (Position -> Double)
fit 1 xs = sum (map sqr xs)
fit 2 xs = sum xs' + (foldr (*) 1 xs')
  where xs' = map abs xs
fit 3 xs = sum [sqr (sum ys) | ys <- tail(inits xs)]
fit 4 xs = maximum (map abs xs)
fit 5 xs = sum (zipWith f xs1 xs)
  where xs1 = tail xs
        f x1 x = 100*sqr (x1-sqr x)+sqr(x-1)
fit 6 xs = fromIntegral (sum (map f xs))
  where f x = sqr(floor (x+0.5))
fit 8 xs = sum (map fit8' xs)
  where fit8' xi = -xi * sin (sqrt (abs xi))
fit 9 xs = sum (map fit9' xs)
  where fit9' xi = sqr xi - 10*cos(2*pi*xi) + 10
fit 10 xs = -20*exp(-0.2*sqrt(sum (map sqr xs)/n')) - exp(sum (map f' xs) / n') + 20 + exp 1
  where n' = fromIntegral (length xs)
        f' xi = cos (2*pi*xi)
fit 11 xs = sum (map sqr xs) / 4000 - prod (zipWith f xs [1..]) + 1
  where f x i = cos (x/sqrt i)
fit 12 xs = (10*sqr(sin (pi*y1))+sum (map f yn1) + sqr(yn-1) )*pi/30 + sum (map fu xs)
   where f y = sqr (y-1)
         y1 = head ys
         yn = last ys
         yn1 = init ys
         ys = map obtainY xs
         obtainY x = 1 + (x+1)/4
         fu x = uf (x,10,100,4)
fit 13 xs = 0.1*(sqr (sin (3*pi*x1)) + sum (zipWith f xs xs1) + sqr(xn-1)*(1+sqr(sin (2*pi*xn)))   ) + sum (map fu xs)
  where xs1 = tail xs
        x1 = head xs
        xn = last xs
        fu x = uf (x,5,100,4)
        f x xx = sqr (x-1) * (1+sqr(sin(3*pi*xx)))                
fit 14 [x1,x2] = 1/(1/500 + sum (map f [0..24]))
  where f j = 1 / (fromIntegral j+1 + (x1 - fa1!!j)**6 + (x2 - fa2!!j)**6)
        fa1 = concat (replicate 5 [-32,-16,0,16,32])
        fa2 = concat (map (replicate 5) [-32,-16,0,16,32])
fit 15 [x1,x2,x3,x4] = sum (zipWith f as bs)
  where f a b = sqr (a-(x1*(sqr b + b*x2)/(sqr b + b*x3 + x4)))
        as = [0.1957,0.1947,0.1735,0.16,0.0844,0.0627,0.0456,0.0342,0.0323,0.0235,0.0246]
        bs = map (1/) [0.25,0.5,1,2,4,6,8,10,12,14,16] 
fit 16 [x,y] = 4*x^2 - 2.1*x^4 + (x^6)/3 + x*y - 4*y^2 + 4*y^4
fit 17 [x,y] = (y - (5.1*x^2/(4*pi^2)) + (5*x)/pi - 6)^2 + 10*(1-1/(8*pi))*cos x +10
fit _ xs = fit 17 xs


-- Auxiliary function for fitness 12 and 13
uf (x,a,k,m) 
           | x > a = k * (x-a)**m
           | x < -a = k * ((-x)-a)**m
           | otherwise = 0


-- Examples of varying number of iterations in each parallel step
iterationsModel :: Int -> [Int]
iterationsModel 1 = replicate 20 50 ++ replicate 40 200
iterationsModel 2 = replicate 2 50 ++ replicate 20 445
iterationsModel 3 = 2:4998:replicate 39 5000
iterationsModel 4 = 2:498:replicate 39 500
iterationsModel 5 = 2:4998:(replicate 9 5000)++(replicate 3 50000)
iterationsModel 6 = 2:4998:(replicate 9 5000)++[50000]
iterationsModel 7 = 2:498:(replicate 9 500)++(replicate 9 5000)
iterationsModel 8 = 2:498:(replicate 99 500)
iterationsModel _ = replicate 50 50


-- Auxiliary functions
sqr x = x*x
prod xs = foldr (*) 1 xs
