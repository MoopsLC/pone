{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell #-}
import System.IO
import Control.Monad
import Data.Monoid
import Data.Functor
import Data.Text (unpack, strip, pack)
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
linesToTest lines = let (x:y:xs) = reverse lines in
    (unlines (reverse xs), drop 8 x, read $ drop 8 y)
    
extract :: Show a => (a -> Bool) -> String -> (a -> (Bool, String))
extract f description = (\x -> (f x, description ++ " got " ++ (show x)))
    
printResult :: TestResult -> String
printResult (Left error) = "Error: " ++ error
printResult (Right (passed, description)) = 
    let passString = if (passed) then "PASS: " else "FAIL: " in
    passString ++ description
  
appendEither :: Monoid a => a -> Either a b -> Either a b
appendEither msg e = case e of 
    Left x -> Left (x `mappend` msg)
    Right x -> e

--todo: use monad transformer (ErrorT?)
testSource :: String -> IO (Either String Var)
testSource source = case parsePone source of
    Left error -> return $ Left error
    Right ast -> do 
        result <- (poneEval ast)
        return $ result

runTest :: PoneTest -> IO TestResult
runTest (Test filename source description expectedValue) = do
    result :: Either String Var <- testSource source
    return $ Right (False, "fixme")--fmap (extract ((==) (PoneInteger expectedValue)) makeString) result
    where makeString = description ++ ": expected " ++ (show expectedValue)
        
isFile :: String -> Bool 
isFile [] = False
isFile ('.':[]) = False
isFile ('.':'.':xs) = False
isFile other = True

combine :: Monoid a => (a -> a) -> a -> a -> a
combine f s r = (f s) `mappend` r

trim :: String -> String
trim s = unpack $ strip $ pack $ s

--todo use writer monad
main = do
    test <- loadTest "C:/Users/M/Desktop/pone/pone_src/test0015.pone"
    case test of 
        Test _ source _ _ -> case parsePone source of
            Left err -> putStrLn err
            Right err -> print $ err
    putStrLn "test"
    runTest test
    --sources :: [FilePath] <- (liftM . filter) isFile $ liftM reverse $ return ["test0006.pone"]--getDirectoryContents root --
    --tests :: [PoneTest] <- mapM (loadTest . (root ++))  sources 
    --results :: [TestResult] <- mapM runTest tests
    --let testResults :: [String] = map ((++ "\n" ) . printResult) results in
    --    putStrLn $ trim $ unlines $ map ((uncurry . combine) (++ "\n")) $ zip sources testResults
    --where root = "C:/Users/M/Desktop/pone/pone_src/"
