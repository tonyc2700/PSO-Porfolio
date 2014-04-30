--import Data.Char
--import Data.Maybe
--import Data.List hiding (foldr)
import Data.Map hiding (map,foldr)
--import Text.Printf
--import Data.Time
--import System.IO  
--import Control.Monad

--module Main(main) where
import PSO
import Data.List
import System.Random
import System.Environment(getArgs)
import Data.Time
--main = do  
--    putStrLn "Hello, what's your name?"  
--    name <- getLine  
--    putStrLn $ "Read this carefully, because this is your future: " ++ name 

--main = do  
--    putStrLn "What's your first name?"  
--    firstName <- getLine  
--    putStrLn "What's your last name?"  
--    lastName <- getLine  
--    let bigFirstName = map toUpper firstName  
--        bigLastName = map toUpper lastName  
--    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?" 

--main = do  
--    return ()  
--    return "HAHAHA"  
--    line <- getLine  
--    return "BLAH BLAH BLAH"  
--    return 4  
--    putStrLn line 

--main = do
--    src <- readFile "grades"
--    --writeFile  "grades.txt" (map toUpper src)
--    --appendFile "grades.txt" ("\nBoo!")
--    ----src <- readFile "grades"
--    start <- getCurrentTime
--    let pairs   = map (split.words) (lines src)
--    print (pairs)
--    let grades  = foldr insert empty pairs
--    mapM_ (draw grades) (sort (keys grades))
--    stop <- getCurrentTime
--    print $ diffUTCTime stop start
--  where
--    insert (s, g) = insertWith (++) s [g]
--    split [name,mark] = (name, read mark)
 
--draw g s = printf "%s\t%s\tAverage: %f\n" s (show marks) avg
--  where
--    marks = findWithDefault (error "No such student") s g
--    avg   = sum marks / fromIntegral (length marks) :: Double

main = do
    src <- readFile "readText.txt"
    let triples   = map (split.words) (lines src)
    let names = extractName triples
    let rateR = extractRate triples ::[Double]
    --getRate rateR
    let expR = extractExp triples ::[Double]
    --getExp expR
    print (portR [0.5,0.5])
    print triples
    --let grades  = foldr insert empty triples
    --mapM_ (draw grades) (sort (keys grades))
    print (extractRate triples)
    --return (getExp expR,getRate rateR)
    return expR
    --let portR w = sum [x*y | x <- w, y <- rateR] :: Position -> [Double] -> Double
  where
    insert (s, g1, g2) = insertWith (++) s [g1,g2]
    split [name,rateR,expR] = (name, read rateR, read expR) :: (String,Double,Double)
    --extractName xs = [d | (d,_,_) <- xs]
    --extractRate xs = [d | (_,d,_) <- xs]
    --extractExp  xs = [d | (_,_,d) <- xs]

extractName xs = [d | (d,_,_) <- xs]
extractRate xs = [d | (_,d,_) <- xs]
extractExp  xs = [d | (_,_,d) <- xs]

--getRate xs = xs :: [Double] -> [Double]
--getExp  xs = xs :: [Double] -> [Double]

rateR :: Position
rateR = [0.01,0.0092]

--Expected return of each asset
expR :: Position
expR = [0.0091,0.0090]

--Return of portfolio
portR :: Position -> Double
portR w = sum [x*y | x <- w, y <- rateR]

--Expected Portfolio return 
expPortR :: Position -> Double
expPortR w = sum [x*y | x <- w, y <- expR]

--main = do
--        contents <- readFile "readText.txt"
--        print . map readInt . words $ contents
---- alternately, main = print . map readInt . words =<< readFile "test.txt"

--readInt :: String -> Int
--readInt = read

--main = do  
--        let list = []
--        handle <- openFile "readText.txt" ReadMode
--        contents <- hGetContents handle
--        let singlewords = words contents
--            list = f singlewords
--        print list
--        print (head list)
--        --hClose handle

--f :: [String] -> [Int]
--f = map read
