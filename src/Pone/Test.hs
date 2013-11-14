module Pone.Test where

import Control.Monad
import System.Timeout
import System.IO
import System.Directory
import System.FilePath
import Debug.Trace

import Pone.Utils
import Pone.Parser
import Pone.Ast

data PoneTest t = Test String --filename
                       String --source
                       String --description
                       String --expectedValue

data Judgement = Judgement String --filename
                           String --test description
                           Bool   --pass or fail
                           String --reason

class Printable a where
    toString :: a -> String

instance Printable Judgement where
    toString (Judgement filename description pass reason) =
        passString ++ ": " ++ name ++ ": " ++ description ++ ": " ++ reason
        where passString = if (pass) then "PASS" else "FAIL"
              name = takeBaseName filename


data TestSuite = All | Single Int

type Timeout = Int
data TestSpec = TestSpec Timeout FilePath TestSuite

type ResultString = String

type TestResult = Either String ResultString

makeJudgement :: TestResult -> PoneTest String -> Judgement
makeJudgement result (Test filename source description expectedValue) =
    either (judge False) (\v -> judge (expectedValue == v) ("expected " ++ expectedValue ++ " got " ++ v)) result
    where judge = Judgement filename description

--eagerly get contents, taken from strict io package
getUtf8 h = hGetContents h >>= \s -> length s `seq` return s

loadTest :: String -> IO (PoneTest String)
loadTest filename = do
    withFile filename ReadMode $ \h -> do
        hSetEncoding h utf8
        contents <- getUtf8 h
        let (source, desc, val) = splitTest contents in
            return $ Test filename source desc val
        where splitTest :: String -> (String, String, String)
              splitTest source = let split = reverse $ lines source in
                  case split of
                      (x:y:xs) -> (unlines (reverse xs), drop 1 x, drop 1 y)
                      _ -> error $ filename ++ "bad test format"

appendError :: String -> Maybe a -> Either String a
appendError msg v = maybeToEither v msg

tryWithTimeout :: String -> Timeout -> IO (Either String t) -> IO (Either String t)
tryWithTimeout msg t proc = do
    result :: Maybe (Either String t) <- timeout (t*1000000) proc
    case result of
        Just value -> return $ value
        Nothing -> return $ Left msg


runTest :: PoneTest String -> IO TestResult
runTest (Test filename source _ _ )  = do
    result <- tryWithTimeout "Interpreter timeout" 3  (return (testSource filename source))
    return $ liftM showAst result
    where testSource :: String -> String -> Either String (PoneProgram (Type Kind))
          testSource filename source = parsePone filename source
          showAst :: PoneProgram (Type Kind) -> String
          showAst ast = show (getExpr ast)

getFiles :: TestSpec -> IO [String]
getFiles (TestSpec t root All) = do
    files :: [String] <- (liftM . filter) isFile $ liftM reverse $ getDirectoryContents root
    return $ map (root ++) files
getFiles (TestSpec t root (Single i)) = return $ [root ++ (intToFile i)]

printResult :: (TestResult, PoneTest String) -> String
printResult (result, test) = toString $ makeJudgement result test

intToFile :: Int -> String
intToFile i = "test" ++ (take (4 - (length s)) $ cycle "0") ++ s ++ ".pone"
              where s = show i

runTestsAndPrint :: TestSpec -> IO [String]
runTestsAndPrint spec = do
    files <- getFiles spec
    tests <- mapM loadTest files
    results <- mapM runTest tests
    return $ map printResult (zip results tests)
