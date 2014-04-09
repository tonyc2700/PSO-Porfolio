--module Main(main) where
import PSO
import Data.List
import System.Random
import System.Environment(getArgs)
import Data.Time
import Data.Map hiding (map,foldr)

-- Example of adjustment parameters for PSO 
-- (taken from M.E.H Pedersen, Tuning & Simplifying Heuristical Optimization)
wpg1 :: (Double,Double,Double)
wpg1 = (-0.16,1.89,2.12)
-- (taken from )
wpg2 :: (Double,Double,Double)
wpg2 = (0.7,1.45,1.49)
--Number of particles
np1 = 40 
--Number of iterations
nit1 = 200
------------------------------------------
--------- Financial variables ------------
------------------------------------------
--Risky characteristic
risk = 0.5
--Risk aversion
aversion = 3
--Required expected return
reqExpR = 0.01
--Penalty parameter-
penPara = 0.1
--Penalty value
penVal = 1/penPara
------------------------------------------
------------------------------------------
------------------------------------------

---------------------------------------------------------------
------- Auxiliary function calling sequential PSO scheme ------
---------------------------------------------------------------

portSeq :: WPGparams -> Int -> Int -> (Position -> Double) -> Boundings -> IO()
portSeq wpg np nit f bo
  = do sg <- getStdGen
       let bestPos = psoSEQ sg wpg np nit f bo
       putStr "Best value: " 
       print (fst bestPos)
       putStr "Best position: "
       print (snd bestPos)

---------------------------------------------------------------
---------------------------------------------------------------

-- Used for testing in an interpreter 
-- mainPortText number of particles, iterations problem and bounds
mainPortTest np nit prob bound = portSeq wpg1 np nit prob bound

main = do
    --putStrLn "Enter file name including extension, eg 'assets.txt'."
    --file <- getLine 
    --src <- readFile file
    src <- readFile "readText.txt"
    let triples   = map (split.words) (lines src)
    let names = extractName triples
    let rateR = extractRate triples :: [Double]
    let expR = extractExp triples :: [Double]
    let nAssets = length rateR
    --------------------Testing----------------------------------------
    ---- Getting settings for portfolio function
    ---- Risk
    --putStrLn "Enter level of risk (0.4-0.9), where 0.4 is least risky."
    --risk' <- getLine
    --let risk = read risk'
    ---- Risk aversion
    --putStrLn "Enter level of risk aversion, recommended 3."
    --aversion' <- getLine
    --let aversion = read aversion'
    ---- Required portfolio return
    --putStrLn "Enter your required portfolio return."
    --reqExpR' <- getLine
    --let reqExpR = read reqExpR'
        --Return of portfolio
    -------------------------------------------------------------------
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
    portSeq wpg1 np1 nit1 mainPortFunction weightBounds
    return ()
  where
    --insert (s, g1, g2) = insertWith (++) s [g1,g2]
    split [name,rateR,expR] = (name, read rateR, read expR) :: (String,Double,Double)
    extractName xs = [d | (d,_,_) <- xs]
    extractRate xs = [d | (_,d,_) <- xs]
    extractExp  xs = [d | (_,_,d) <- xs]


outPutFile names pos = do
    t <- getCurrentTime
    appendFile ("output-" ++ (time t)) ((time t) ++ "\nBoo\n")
  where time t = show (toGregorian $ utctDay t)

-- Turns a number into a percentage 
toPerc :: Double -> Double
toPerc x = 100*(myRound x 4)
-- Rounds a number to s decimal points 
myRound n s = fromIntegral (round (n * factor)) / factor
    where factor = fromIntegral (10^s)

--printStuff :: String -> [Double] -> String
--printStuff (x:xs) (y:ys) = x 

--test1 = do
--    mapM_ (draw grades) (sort (keys grades))

--draw g s = printf "%s\t%s\tAverage: %f\n" s (show marks) avg
--  where
--    marks = findWithDefault (error "No such student") s g
--    avg   = sum marks / fromIntegral (length marks) :: Double