{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell #-}
import System.IO
import Data.Monoid
import Data.Functor
import Data.Tuple.HT (uncurry3)
import qualified Data.Map as Map

import Pone.Parser
import Pone.Interpreter
import Pone.Ast

import Debug.Trace
import System.Directory
    
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

runTest :: PoneTest -> IO TestResult
runTest (Test filename source description expectedValue) = do
    result :: Either String Integer <- testSource source
    return $ fmap (extract ((==) expectedValue) makeString) result
    where makeString = description ++ ": expected " ++ (show expectedValue)

    
assembleResult :: TestResult -> String
assembleResult (Left error) = "Error: " ++ error
assembleResult (Right (passed, description)) = 
    let passString = if (passed) then "PASS: " else "FAIL: " in
    passString ++ description
       
-- Either String a -> (a -> IO (Either String b)) -> IO (Either String b)
       -- parsePone source >>= poneEval
-- m a -> (a -> m b) -> m b
testSource :: String -> IO (Either String Integer)
testSource source = case parsePone source of
    Left error -> return $ Left error
    Right ast -> poneEval ast


-- Either String PoneProgram
    -- fromIo :: Either String Integer <- poneEval ast
    -- return fromIo
-- return $ fmap poneEval ast
    -- where ast = parsePone source

isFile :: FilePath -> Bool
isFile [] = False
isFile ('.':[]) = False
isFile ('.':'.':xs) = False
isFile other = True

main = do
    sources :: [FilePath] <- getDirectoryContents root
    tests :: [PoneTest] <- sequence $ map loadTest $ map (root ++ ) $ filter isFile sources 
    results :: [TestResult] <- sequence $ map runTest tests
    print $ map assembleResult results
    where root = "C:/Users/M/Desktop/pone/pone_src/"
