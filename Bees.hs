-------------------------------------------------------------
-- Generic scheme to deal with Artificial Bee Colony       --
-- The code contains a generic sequential Haskell function --
-- (beesSEQ) as well as a parallel implementations in      --
-- Eden (beesPAR).                                         --
-- Some example functions are also included (but they are  --
-- not exported).                                          --
--                                                         --
-- Created by: A.Encina, P.Rabanal, I.Rodriguez,F. Rubio   --
-- Last modified: April 2013                               --
-------------------------------------------------------------

module Bees(beesSEQ,                              -- Sequential ABC scheme
            beesPAR,                              -- Parallel versions of PSO
            Position,Speed,Boundings,Params,Bee   -- Auxiliary types
            ) where
import Control.Parallel.Eden
import Data.List
import System.Random


----------------------------
-- Basic types to be used --
----------------------------

-- Types and functions dealing with R^n positions
type Position = [Double]     -- Assuming R^n
type Speed = Position        
type Boundings = [(Double,Double)]  -- (Lower,upper) for each dimension

infixl 7 *& 
(*&) :: Double -> Speed -> Speed
x *& xs = map (x*) xs

infixl 6 -&
(-&) :: Speed -> Speed -> Speed
xs -& ys = zipWith (-) xs ys

infixl 6 +&
(+&) :: Speed -> Speed -> Speed
xs +& ys = zipWith (+) xs ys

-- Parameters
type Params = (Int)      -- L

-- Particle: Best local value, best global value, current position, scouting counter L
type Bee = (Double,Double,Position,Int)


-----------------------------------
-- Generic ABC sequential scheme --
-----------------------------------

-- General sequential bees scheme
beesSEQ :: RandomGen a => a           -- Random generator
          -> Params                   -- Standard adjustment parameters
          -> Int                      -- Number of bees to be used
          -> Int                      -- Maximum number of iterations
          -> (Position -> Double)     -- Fitness function
          -> Boundings                -- Search space boundaries 
          -> (Double,Position)        -- Value and position of best fitness
beesSEQ sg lp nb it f bo = obtainBestBee (bees' sg2 lp it f bo initBees)
  where initBees :: [Bee]
        initBees = initializeBees sg1 nb bo f lp
        (sg1,sg2) = split sg

-- Sequential function taking care of the execution of a given number of iterations it
-- of the basic ABC algorithm. It is used both from the sequential and parallel schemes.
bees' _ _ 0 _ _ bs = bs
bees' sg nl it f bo bs = bees' sg2 nl (it-1) f bo newBees 
  where newBees = oneStepBee sg1 nl f bo bs
        (sg1,sg2) = split sg

-- Basic sequential function implementing one step of the basic ABC algorithm
oneStepBee :: RandomGen a => a -> Params -> (Position -> Double) -> Boundings
               -> [Bee] -> [Bee]
oneStepBee sg nl f bo bs = abandoned sg onlookersBs
-- Basic structure: (abandoned . onlookers . normalize . employed) bs
 where
   (sg1,sgAux) = split sg
   (sg2,sg3) = split sgAux
   rs :: [(Int,Int,Double)] -- (Int,Int,Double): ([0..numBees-1],[0..numDimensions-1],[-1..1])
   rs = genRanIndexDimR sg1 (length bs -1 ) (length bo -1)
   
   employedBsAux = zipWith move bs rs
   employedBs = updateBest bestBeeEmployed employedBsAux
   bestBeeEmployed = global (maximum employedBsAux)
   move (lv,gv,pos,l) (j,dim,fi)
    | lv' > gv = (lv',lv',pos',nl)
    | lv' > lv = (lv',gv,pos',nl)
    | otherwise = (lv,gv,pos,l-1) 
     where (pl,pd:pr) = splitAt dim pos
           pd' = pd + fi*(pd - (position (bs!!j) !! dim))
           pos' = limitRange bo (pl++(pd':pr))
           lv' = f pos'

   normFitness = normalize (map value employedBs)
   normIndexes = take (length bs) (map ((indexNormalized normFitness).fst3) rOs)
   rOs = genRanDimPos sg2 (length bo - 1) bo
   onlookersBsAux = moveOnlookers f nl (zip normIndexes rOs) employedBs 
   onlookersBs = updateBest bestOnlooker onlookersBsAux
   bestOnlooker = global (maximum employedBsAux)

   abandoned sg bs 
     | null abs = okBs
     | bestNew > bestOld = newBs ++ okBs'
     | otherwise = newBs' ++ okBs
     where (abs,okBs) = filter' ((<1).counter) bs
           newBs = initializeBees sg3 (length abs) bo f nl
           (_,bestNew,_,_) = head newBs
           (_,bestOld,_,_) = head okBs
           newBs' = updateBest bestOld newBs
           okBs' = updateBest bestNew okBs


moveOnlookers f nl [] bs = bs
moveOnlookers f nl ((j,(_,i,newX)):js) bs 
  = moveOnlookers f nl js newBs
  where newBee = moveOnlooker f nl bee (j,i,newX)
        (lBs,bee:rBs) = splitAt j bs
        newBs = lBs++(newBee:rBs)
moveOnlooker f nl (lv,gv,pos,l) (j,dim,newX)
    | lv' > gv = (lv',lv',pos',nl)
    | lv' > lv = (lv',gv,pos',nl)
    | otherwise = (lv,gv,pos,l-1) 
     where (pl,pd:pr) = splitAt dim pos
           pos' = pl++(newX:pr)
           lv' = f pos'

updateBest best bs = map (updateGlobalBest best) bs
  where updateGlobalBest newBest (a,b,c,d) = (a,newBest,c,d)

-- Obtaining random indexes and positions
genRanIndexDimR :: RandomGen a => a -> Int -> Int -> [(Int,Int,Double)]
genRanIndexDimR sg numBees numDim = zip3 indexs dims rs
  where (sg1,sg2) = split sg
        indexs = randomRs (0,numBees) sg
        dims   = randomRs (0,numDim) sg1
        rs     = randomRs (-1,1) sg2        
genRanDimPos :: RandomGen a => a -> Int -> Boundings -> [(Double,Int,Double)]
genRanDimPos sg numDim bo = zip3 indexs dim xs
  where (sg1,sg2) = split sg 
        indexs = randomRs (0,1) sg
        dim = randomRs (0,numDim) sg1
        xs = randomRs (bomin,bomax) sg2
        bomin = minimum (map fst bo)
        bomax = maximum (map snd bo)

-- Normalizing functions		
normalize :: [Double] -> [Double]
normalize xs = scanl1 (+) (map (/total) xs)
  where total = sum xs
indexNormalized :: [Double] -> Double -> Int
indexNormalized xs x = length (takeWhile (x>) xs) 

                
-- Initialization of bees				
initializeBees sg nb bo f lp = map (addBestL bestValue) nearlyBees
  where ndim = length bo
        pos :: [Position] 
        pos = take nb (randomPs (ndim*nb) bo sg)
        fs = map f pos
        
        nearlyBees :: [(Double,Position)]
        nearlyBees = zip fs pos
        bestValue = (fst . maximum) nearlyBees

        addBestL bv (fv,po) = (fv,bv,po,lp)

obtainBestBee :: [Bee] -> (Double,Position)
obtainBestBee bees = (bg{-bv-},bp)
  where (bv,bg,bp,_) = maximum bees


---------------------------------
-- Generic ABC parallel scheme --
---------------------------------

-- General parallel ABC scheme (version 1)
beesPAR :: RandomGen a => a            -- Random generator
           -> Params                   -- Standard adjustment parameters
           -> Int                      -- Number of bees to be used
           -> Int                      -- Iterations in each parallel step
           -> Int                      -- Number of parallel iterations
           -> Int                      -- Number of parallel processes
           -> (Position -> Double)     -- Fitness function
           -> Boundings                -- Search space boundaries 
           -> (Double,Position)        -- Value and position of best fitness
beesPAR sg lp nb pit it nPE f bo = obtainBestBee (last poutsFlat)
  where initBees :: [Bee]
        initBees = initializeBees sg nb bo f lp
        
        sgs = tail (generateSGs (nPE+1) sg)
        
        pouts :: [ [[Bee]] ] 
        pouts = [process (beesP (sgs!!i) lp pit f bo) # (take it (pins!!i)) | i<-[0..nPE-1]]   `using` spine

        poutsFlat = flatXsss pouts 
        pins = unFlat nPE (initBees : poutsFlat)

flatXsss [] = []
flatXsss ([]:_) = []
flatXsss xsss = concatUpdate (map head xsss) : flatXsss (map tail xsss)
unFlat np [] = replicate np []
unFlat np (xs:xss) = lzipWith (:) firsts rest
  where firsts = shuffle np xs
        rest = unFlat np xss
concatUpdate xss = updateBest best (concat xss)
  where best = maximum (map (global.head) xss)

		
-- Basic process function used by the parallel scheme
beesP :: RandomGen a => a 
        -> Params -> Int -> (Position -> Double) -> Boundings
        -> ([[Bee]] -> [[Bee]])
beesP sg lp pit f bo [] = []
beesP sg lp pit f bo (bs:bss)
  = bees' sg1 lp pit f bo bs : beesP sg2 lp pit f bo bss 
  where (sg1,sg2)=split sg

  
-------------------------
-- Auxiliary functions --
-------------------------  
        
shuffle n xs 
  | null dr = take n (map (:[]) iz ++ repeat [])
  | otherwise = zipWith (:) iz (shuffle n dr)
  where (iz,dr) = splitAt n xs

spine [] = ()
spine (x:xs) = spine xs

position (lv,gv,pos,l) = pos
counter (lv,gv,pos,l) = l
value (lv,gv,pos,l) = lv
global (lv,gv,pos,l) = gv
        
limitRange bo xs = zipWith limit1 bo xs
limit1 (l,u) n = min (max n l) u

lzipWith f [] _ = []
lzipWith f (x:xs) ~(y:ys) = f x y : lzipWith f xs ys

fst3 (x,y,z) = x

filter' f [] = ([],[])
filter' f (x:xs)
  | f x = (x:le,ri)
  | otherwise = (le,x:ri)
  where (le,ri) = filter' f xs

generateSGs 0 sg = []
generateSGs 1 sg = [sg]
generateSGs n sg = sg1:generateSGs (n-1) sg2
  where (sg1,sg2) = split sg

randomPs n bo sg = transpose (map (take n) xss')
  where xss = map (flip randomRs sg) bo
        xss' = zipWith drop [0,n..] xss


-------------------------------
-- CONCRETE RUNNING EXAMPLES --
-------------------------------

-- Examples of fitness functions with corresponding boundings. taken from Yao et al 
-- (Evolutionary Programming made faster, IEEE Trans. on Evolutionary Computation)

bo1 = replicate 30 (-100,100)
fit1 xs = sum (map sqr xs)

bo2 = replicate 30 (-10,10)
fit2 xs = sum xs' + (foldr (*) 1 xs')
  where xs' = map abs xs

bo3 = replicate 30 (-100,100)
fit3 xs = sum [sqr (sum ys) | ys <- tail(inits xs)]

bo4 = replicate 30 (-100,100)
fit4 xs = maximum (map abs xs)

bo5 = replicate 30 (-30,30)
fit5 xs = sum (zipWith f xs1 xs)
  where xs1 = tail xs
        f x1 x = 100*sqr (x1-sqr x)+sqr(x-1)

bo6 = replicate 30 (-100,100)
fit6 xs = fromIntegral (sum (map f xs))
  where f x = sqr(floor (x+0.5))
sqr x = x*x

bo8 = replicate 30 (-500,500)
fit8 xs = sum (map fit8' xs)
  where fit8' xi = -xi * sin (sqrt (abs xi))

bo9 = replicate 30 (-5.12,5.12)
fit9 xs = sum (map fit9' xs)
  where fit9' xi = sqr xi - 10*cos(2*pi*xi) + 10

bo10 = replicate 30 (-32,32)
fit10 xs = -20*exp(-0.2*sqrt(sum (map sqr xs)/n')) - exp(sum (map f' xs) / n') + 20 + exp 1
  where n' = fromIntegral (length xs)
        f' xi = cos (2*pi*xi)

bo11 = replicate 30 (-600,600)
fit11 xs = sum (map sqr xs) / 4000 - prod (zipWith f xs [1..]) + 1
  where f x i = cos (x/sqrt i)

prod xs = foldr (*) 1 xs

bo12 = replicate 30 (-50,50)
fit12 xs = (10*sqr(sin (pi*y1))+sum (map f yn1) + sqr(yn-1) )*pi/30 + sum (map fu xs)
   where f y = sqr (y-1)
         y1 = head ys
         yn = last ys
         yn1 = init ys
         ys = map obtainY xs
         obtainY x = 1 + (x+1)/4
         fu x = uf (x,10,100,4)
uf (x,a,k,m) 
           | x > a = k * (x-a)**m
           | x < -a = k * ((-x)-a)**m
           | otherwise = 0

bo13 = replicate 30 (-50,50)
fit13 xs = 0.1*(sqr (sin (3*pi*x1)) + sum (zipWith f xs xs1) + sqr(xn-1)*(1+sqr(sin (2*pi*xn)))   ) + sum (map fu xs)
  where xs1 = tail xs
        x1 = head xs
        xn = last xs
        fu x = uf (x,5,100,4)
        f x xx = sqr (x-1) * (1+sqr(sin(3*pi*xx)))         
         
fit14 [x1,x2] = 1/(1/500 + sum (map f [0..24]))
  where f j = 1 / (fromIntegral j+1 + (x1 - fa1!!j)**6 + (x2 - fa2!!j)**6)
        fa1 = concat (replicate 5 [-32,-16,0,16,32])
        fa2 = concat (map (replicate 5) [-32,-16,0,16,32])
bo14 = replicate 2 (-65536,65536)

fit15 [x1,x2,x3,x4] = sum (zipWith f as bs)
  where f a b = sqr (a-(x1*(sqr b + b*x2)/(sqr b + b*x3 + x4)))
        as = [0.1957,0.1947,0.1735,0.16,0.0844,0.0627,0.0456,0.0342,0.0323,0.0235,0.0246]
        bs = map (1/) [0.25,0.5,1,2,4,6,8,10,12,14,16] 
bo15 = replicate 4 (-5,5)

fit16 [x,y] = 4*x^2 - 2.1*x^4 + (x^6)/3 + x*y - 4*y^2 + 4*y^4
bo16 = [(-5,5),(-5,5)]

fit17 [x,y] = (y - (5.1*x^2/(4*pi^2)) + (5*x)/pi - 6)^2 + 10*(1-1/(8*pi))*cos x +10
bo17 = [(-5,10),(0,15)]

inverseFit :: (Position -> Double) -> (Position -> Double)
inverseFit f x
  | fx < 0    = 1 + abs fx
  | otherwise = 1 / (1+fx)  
  where fx = f x
