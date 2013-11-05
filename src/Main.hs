{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell #-}
import System.IO
import Data.Monoid
import Data.Functor
import Data.Tuple.HT (uncurry3)
import qualified Data.Map as Map

import Pone.Parser
import Pone.Interpreter
import Debug.Trace

import Control.Lens

sources = ["C:/Users/M/Desktop/pone/pone_src/test.pone"
          ,"C:/Users/M/Desktop/pone/pone_src/test2.pone"
          ] --todo load test results from disk

    
data PoneTest = Test String String String Integer
type TestResult = Either String (Bool, String)

loadTest :: String -> IO PoneTest
loadTest filename = do
    contents <- readFile filename
    return $ makeTest filename (linesToTest (lines contents))

makeTest :: String -> (String, String, Integer) -> PoneTest
makeTest name (source, desc, val) = Test name source desc val
  
linesToTest :: [String] -> (String, String, Integer)
linesToTest (x:y:xs) = (unlines xs, drop 8 y, read $ drop 8 x)
    

extract :: Show a => (a -> Bool) -> String -> (a -> (Bool, String))
extract f description = (\x -> (f x, description ++ " got " ++ (show x)))

runTest :: PoneTest -> IO (Either String (Bool, String))
runTest (Test filename source description expectedValue) = do
    result :: Either String Integer <- testSource source
    return $ fmap (extract ((==) expectedValue) makeString) result
    where makeString = description ++ ": expected " ++ (show expectedValue)

    
assembleResult :: TestResult -> String
assembleResult (Left error) = "Error: " ++ error
assembleResult (Right (passed, description)) = 
    let passString = if (passed) then "PASS: " else "FAIL: " in
    passString ++ description
       
testSource :: String -> IO (Either String Integer)
testSource source = return $ fmap poneEval $ parsePone source

main = do
    tests :: [PoneTest] <- sequence $ map loadTest sources 
    results :: [TestResult] <- sequence $ map runTest tests
    print $ map assembleResult results

