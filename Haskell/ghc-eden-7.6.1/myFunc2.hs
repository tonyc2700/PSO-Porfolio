--module Main(main) where
import PSO
import Data.List
import System.Random
import System.Environment(getArgs)
import Data.Time
import Data.Map hiding (map,foldr)

main = do
    putStrLn "Enter file name including extension, eg 'assets.txt'"
    file <- getLine 
    src <- readFile file
    let triples   = map (split.words) (lines src)
    let names = extractName triples
    let rateR = extractRate triples :: Position
    let expR = extractExp triples :: Position
    let nAssets = length rateR
    -- Example of adjustment parameters 
    -- (taken from M.E.H Pedersen, Tuning & Simplifying Heuristical Optimization)
    let wpg1 = (-0.16,1.89,2.12) :: (Double,Double,Double)
    --Risky characteristic
    let risk = 0.5
    --Risk aversion
    let aversion = 3
    --Required expected return
    let reqExpR = 0.004
    --Return of portfolio
        portR w = sum [x*y | x <- w, y <- rateR] :: Position -> Double
    --Expected Portfolio return 
    let expPortR w = sum [x*y | x <- w, y <- expR] :: Position -> Double
    --Penalty parameter
    let penPara = 0.1
    --Penalty value
    let penVal = 1/penPara
    --Portfolio function
    let port w = risk*(max 0 ((portR w)-(expPortR w)))+(1-risk)*(((max 0 ((expPortR w)-(portR w))))**(1/aversion))-(expPortR w) :: Position -> Double
    --Unconstrained porfolio function
    let mainPortFunction w = (port w) + penVal*((expPortR w)-reqExpR) + penVal*(abs((sum w) - 1)) :: Position -> Double
    portSeq wpg1 20 1000 mainPortFunction (replicate nAssets (0.05,0.35))
  where
    insert (s, g1, g2) = insertWith (++) s [g1,g2]
    split [name,rateR,expR] = (name, read rateR, read expR) :: (String,Double,Double)
    extractName xs = [d | (d,_,_) <- xs]
    extractRate xs = [d | (_,d,_) <- xs]
    extractExp xs = [d | (_,_,d) <- xs]

-- Boundings for the weights 
--weightsBound :: Boundings
--weightsBound = replicate nAssets (0.05,0.35)

portSeq :: WPGparams -> Int -> Int -> (Position -> Double) -> Boundings -> IO()
portSeq wpg np nit f bo
  = do sg <- getStdGen
       --start <- getCurrentTime
       let bestPos = psoSEQ sg wpg np nit f bo
       putStr "Best value: " 
       print (fst bestPos)
       putStr "Best position: "
       print (snd bestPos)
       putStr "Sum of weights: "
       print (sum (snd bestPos))
       --putStr "Expected Portfolio Return: "
       --print (portR (snd bestPos))
       --appendFile "testing3.txt" (show bestPos)
       --stop <- getCurrentTime
       --appendFile "testing3.txt" (show (diffUTCTime stop start) ++ "\n")



{--

--module Main(main) where
import PSO
import Data.List
import System.Random
import System.Environment(getArgs)
import Data.Time
import Data.Map hiding (map,foldr)

main = do
    putStrLn "Enter file name including extension, eg 'assets.txt'"
    file <- getLine 
    src <- readFile file
    let triples   = map (split.words) (lines src)
    let names = extractName triples
    let rateR = extractRate triples :: [Double]
    print rateR
    let expR = extractExp triples :: [Double]
    print expR
    let nAssets = length rateR
        --Return of portfolio
        portR :: Position -> Double
        portR w = sum [x*y | x <- w, y <- rateR]
        --Expected Portfolio return 
        expPortR :: Position -> Double
        expPortR w = sum [x*y | x <- w, y <- expR]
        --Portfolio function
        port :: Position -> Double
        port w = risk*(max 0 ((portR w)-(expPortR w)))+(1-risk)*(((max 0 ((expPortR w)-(portR w))))**(1/aversion))-(expPortR w)
        --Unconstrained porfolio function 
        mainPortFunction :: Position -> Double
        --mainPortFunction w = (port w) + penVal*(abs((expPortR w)-reqExpR)) + penVal*(abs((sum w) - 1))
        mainPortFunction w = (port w) + penVal*((expPortR w)-reqExpR) + penVal*(abs((sum w) - 1))
        weightBounds = replicate nAssets (0.05,0.35)
        --Unconstrained porfolio function 
    --print (weightBounds)
    --putStrLn "Boooo!"
    mainPortTest 40 100 mainPortFunction weightBounds
    return ()
  where
    insert (s, g1, g2) = insertWith (++) s [g1,g2]
    split [name,rateR,expR] = (name, read rateR, read expR) :: (String,Double,Double)
    extractName xs = [d | (d,_,_) <- xs]
    extractRate xs = [d | (_,d,_) <- xs]
    extractExp  xs = [d | (_,_,d) <- xs]
    ----Penalty parameter-
    --penPara = 0.1
    ----Penalty value
    --penVal = 1/penPara


-- Example of adjustment parameters 
-- (taken from M.E.H Pedersen, Tuning & Simplifying Heuristical Optimization)
wpg1 :: (Double,Double,Double)
wpg1 = (-0.16,1.89,2.12)

--Risky characteristic
risk = 0.5
--Risk aversion
aversion = 3
--Required expected return
reqExpR = 0.01
--Number of assets
nAssets = 10

----Return of each asset
--rateR :: Position
--rateR = [0.01,0.0092,0.017,0.0041,0.012,0.007,0.01,0.032,0.005,0.03]

----Expected return of each asset
--expR :: Position
--expR = [0.0091,0.0090,0.0107,0.0041,0.0118,0.001,0.007,0.03,0.003,0.015]

----Return of portfolio
--portR :: Position -> Double
--portR w = sum [x*y | x <- w, y <- rateR]

----Expected Portfolio return 
--expPortR :: Position -> Double
--expPortR w = sum [x*y | x <- w, y <- expR]

----Portfolio function
--port :: Position -> Double
--port w = risk*(max 0 ((portR w)-(expPortR w)))+(1-risk)*(((max 0 ((expPortR w)-(portR w))))**(1/aversion))-(expPortR w)

--Penalty parameter-
penPara = 0.1
--Penalty value
penVal = 1/penPara

----Unconstrained porfolio function 
--mainPortFunction :: Position -> Double
----mainPortFunction w = (port w) + penVal*(abs((expPortR w)-reqExpR)) + penVal*(abs((sum w) - 1))
--mainPortFunction w = (port w) + penVal*((expPortR w)-reqExpR) + penVal*(abs((sum w) - 1))

--weightBounds = replicate nAssets (0.05,0.35)

portSeq :: WPGparams -> Int -> Int -> (Position -> Double) -> Boundings -> IO()
portSeq wpg np nit f bo
  = do sg <- getStdGen
       let bestPos = psoSEQ sg wpg np nit f bo
       putStr "Best value: " 
       print (fst bestPos)
       putStr "Best position: "
       print (snd bestPos)

-- mainPortText number of particles, iterations problem and bounds
mainPortTest np nit prob bound = portSeq wpg1 np nit prob bound


--}