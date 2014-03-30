-------------------------------------------------------------
-- Generic scheme to deal with Artificial Bee Colony       --
--                                                         --
-- This file contains examples of fitness functions, as    --
-- well as examples of main functions to call to the       --
-- generic ABC schemes defined in module PSO.              -- 
--                                                         --
-- Created by: A.Encina, P.Rabanal, I.Rodriguez,F. Rubio   --
-- Last modified: April 2013                               --
-------------------------------------------------------------

module Main(main) where
import Bees
import Data.List
import System.Random
import System.Environment(getArgs)


------------------------------------------
-- Main function including input/output --
------------------------------------------

mainBeesSeq :: Int -> Int -> (Position -> Double) -> Boundings -> IO ()
mainBeesSeq nb nit f bo 
  = do sg <- getStdGen
       let nl = nb * length bo
           bestPos = beesSEQ sg nl nb nit (inverseFit f) bo
       print (bestPos,f (snd bestPos))

mainBeesPar :: Int -> Int -> Int -> Int -> (Position -> Double) -> Boundings -> IO ()
mainBeesPar nb pit it np f bo 
  = do sg <- getStdGen
       let nl = nb * length bo
           bestPos = beesPAR sg nl nb pit it np (inverseFit f) bo           
       print (bestPos,f (snd bestPos))

	   
-- Main function that can call to the sequential or the parallel scheme
-- Examples of use (assumming the executable to be a.out):
--           
--    ./a.out 3 1 100 5000 +RTS -N1
--           (problem number 3) (version 1: sequential) (100 bees) (5000 iterations) (1 processor)
--    ./a.out 1 2 100 100 15 2 +RTS -N2
--           (problem number 1) (version 2: parallel) (100 bees) 
--           (100 iterations per parallel step) (15 parallel steps) (2 islands) (2 processors)
--	   
main = do args <- getArgs
          let (fnumber:version:nb:args') = map read args		
              (itSeq:_) = args'		
              [pit,it,np] = args'		
          case version of
		1 -> mainBeesSeq nb itSeq (fit fnumber) (bo fnumber)	
                2 -> mainBeesPar nb pit it np (fit fnumber) (bo fnumber)

		  
-------------------------------
-- CONCRETE RUNNING EXAMPLES --
-------------------------------

-- Examples of fitness functions with corresponding boundings. taken from Yao et al 
-- (Evolutionary Programming made faster, IEEE Trans. on Evolutionary Computation)
bo :: Int -> Boundings
bo 1 = replicate 30 (-100,100)
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

-- Auxiliary functions
sqr x = x*x
prod xs = foldr (*) 1 xs

inverseFit :: (Position -> Double) -> (Position -> Double)
inverseFit f x
  | fx < 0    = 1 + abs fx
  | otherwise = 1 / (1+fx)  
  where fx = f x
