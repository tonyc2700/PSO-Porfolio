--module Main(main) where
import PSO
import Data.List
import System.Random
import System.Environment(getArgs)
import Data.Time
import Data.Map hiding (map,foldr)
import Text.Printf

-- Example of adjustment parameters for PSO 
-- (taken from M.E.H Pedersen, Tuning & Simplifying Heuristical Optimization)
wpg1 :: (Double,Double,Double)
wpg1 = (-0.16,1.89,2.12)
-- (taken from )
wpg2 :: (Double,Double,Double)
wpg2 = (0.7,1.45,1.49)

----Example Number of particles
--np1 = 100 -- for testing
----Example Number of iterations
--nit1 = 1000-- for testing

--------------------------------------------
----------- Financial variables ------------
--------------------------------------------
----Risky characteristic
--risk = 0.5 -- for testing
----Risk aversion
--aversion = 3 -- for testing
----Required expected return
--reqExpR = 0.01 -- for testing
--------------------------------------------
--------------------------------------------
--Penalty parameter
penPara = 0.1
--Penalty value
penVal = 1/penPara
--------------------------------------------
--------------------------------------------
--------------------------------------------

---------------------------------------------------------------
------- Auxiliary function calling sequential PSO scheme ------
---------------------------------------------------------------
portSeq :: [String] -> WPGparams -> Int -> Int -> (Position -> Double) -> Boundings -> IO()
portSeq names wpg np nit f bo
  = do sg <- getStdGen
       let bestPos = psoSEQ sg wpg np nit f bo
       putStr "Best value: " 
       print (fst bestPos)
       putStr "Best position: "
       print (snd bestPos)
       outPutFile names (snd bestPos)
---------------------------------------------------------------
---------------------------------------------------------------

main = do
    --Getting the name of the file with asset information
    putStrLn "Enter file name including extension, eg 'assets.txt'."
    file <- getLine 
    src <- readFile file
    --src <- readFile "readText.txt" -- for testing
    let triples   = map (split.words) (lines src)
    let names = extractName triples :: [String]
    let rateR = extractRate triples :: [Double]
    let expR = extractExp triples :: [Double]
    let nAssets = length rateR
    -------------------For Testing--------------------------------------
    -- Getting settings for PSO
    -- Number of particles for PSO
    putStrLn "Enter the number of particles for the swarm."
    np' <- getLine
    let np = read np'
    -- Number of iteration for PSO
    putStrLn "Enter the number of iterations for the PSO to run."
    nit' <- getLine
    let nit = read nit'
    -- Getting settings for portfolio function
    -- Risk
    putStrLn "Enter level of risk (0.4-0.9), where 0.4 is least risky."
    risk' <- getLine
    let risk = read risk'
    -- Risk aversion
    putStrLn "Enter a level for risk aversion, recommended 3."
    aversion' <- getLine
    let aversion = read aversion'
    -- Required portfolio return
    putStrLn "Enter your required portfolio return, eg '0.02'."
    reqExpR' <- getLine
    let reqExpR = read reqExpR'
    -------------------------------------------------------------------
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
        --Weight bound is set to this to induce diversification
        weightBounds = replicate nAssets (0.05,0.35)
    let pso = portSeq names wpg1 np nit mainPortFunction weightBounds
    --outPutFile names (snd pso)
    pso
    return ()
  where
    --insert (s, g1, g2) = insertWith (++) s [g1,g2]
    split [name,rateR,expR] = (name, read rateR, read expR) :: (String,Double,Double)
    extractName xs = [d | (d,_,_) <- xs] :: [String]
    extractRate xs = [d | (_,d,_) <- xs]
    extractExp  xs = [d | (_,_,d) <- xs]
    portReturn w e = sum [x*y | x <- w, y <- e]

--Used to print the results from the PSO to a file
--It either creates or changes a file.
outPutFile names pos = do
    t <- getCurrentTime
    appendFile ("output-" ++ (time t)) ((time t) ++ "\nOptimal Portfolio:\n" ++ (printStuff names pos) ++ "\n" )
    --appendFile ("output-" ++ (time t)) ("Expected Portfolio Return is: " ++ "\n\n" )
  where time t = show (toGregorian $ utctDay t)

printStuff [] [] = []
printStuff (n:ns) (p:ps) = n ++ "\t" ++ toPerc p ++ "% \n" ++ (printStuff ns ps)

-- Turns a number into a percentage 
toPerc :: Double -> String
toPerc = printf "%.2f" . (*100)

---- Turns a number into a percentage 
--toPerc :: Double -> Double
--toPerc x = 100*(myRound x 4)
---- Rounds a number to s decimal points 
--myRound n s = fromIntegral (round (n * factor)) / factor
--    where factor = fromIntegral (10^s)
