{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell #-}
import System.IO
import Control.Monad
import Data.Monoid
import Data.Functor
import Data.Text (unpack, strip, pack)
import Text.Printf (printf)
import Data.Tuple.HT (uncurry3)
import qualified Data.Map as Map

import Pone.Parser
import Pone.Interpreter
import Pone.Ast

import Debug.Trace
import System.Directory
import System.Timeout

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

--monad transformer -> Maybe o Either String
tryWithTimeout :: String -> Int -> IO (Either String a) -> IO (Either String a)
tryWithTimeout msg s proc = do
    result <- timeout (s*1000000) proc
    case result of
        Just value -> return $ value
        Nothing -> return $ Left msg

runTest :: PoneTest -> IO TestResult
runTest (Test filename source description expectedValue) = do
    result :: Either String Var <- tryWithTimeout "Interpreter timeout" 3 (testSource source)
    return $ fmap (extract ((==) (PoneInteger expectedValue)) makeString) result
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

root = "C:/Users/M/Desktop/pone/pone_src/"

runOne :: Int -> IO ()
runOne num =
    let zeros = (if (num < 10) then "000" else "00") in do
        print num
        test <- loadTest ( root ++ "test" ++ zeros ++ (show num) ++ ".pone")
        case test of
            Test _ source _ _ -> case parsePone source of
                Left err -> putStrLn err
                Right err -> print $ err
        join $ liftM (print . printResult) (runTest test)


--todo use writer monad
runAll :: IO ()
runAll = do
    sources :: [FilePath] <- (liftM . filter) isFile $ liftM reverse $ getDirectoryContents root
    tests :: [PoneTest] <- mapM (loadTest . (root ++))  sources
    results :: [TestResult] <- mapM runTest tests
    let testResults :: [String] = map ((++ "\n" ) . printResult) results in
        putStrLn $ trim $ unlines $ map ((uncurry . combine) (++ "\n")) $ zip sources testResults

doRunOne = False
main = do
    if doRunOne
        then runOne 7
        else runAll