module Pone.Testing where

import Control.Monad
import System.Timeout
import System.IO
import System.Directory
import System.FilePath

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

class Print a where
    toString :: a -> String

instance Print Judgement where
    toString (Judgement filename description pass reason) =
        passString ++ ": " ++ name ++ ": " ++ description ++ ": " ++ reason
        where passString = if (pass) then "PASS" else "FAIL"
              name = takeBaseName filename


data TestSuite = Single Int
               | All

type Timeout = Int
data TestSpec = TestSpec Timeout FilePath TestSuite

type ResultString = String

type TestResult = Either String ResultString

makeJudgement :: TestResult -> PoneTest String -> Judgement
makeJudgement result (Test filename source description expectedValue) =
    case result of
        Left err -> judge False err
        Right resValue ->
            let eq = expectedValue == resValue in
                judge eq ("expected " ++ expectedValue ++ " got " ++ resValue)

    where judge = Judgement filename description

loadTest :: String -> IO (PoneTest String)
loadTest filename = do
    contents <- readFile filename
    let (source, desc, val) = splitTest contents in
        return $ Test filename source desc val
    where splitTest :: String -> (String, String, String)
          splitTest source = let split = lines source in
              case split of
                  (x:y:xs) -> (unlines (reverse xs), drop 1 x, drop 1 y)
                  _ -> undefined

tryWithTimeout :: String -> Timeout -> IO (Either String t) -> IO (Either String t)
tryWithTimeout msg t proc = do
    result <- timeout (t*1000000) proc
    case result of
        Just value -> return $ value
        Nothing -> return $ Left msg



runTest :: PoneTest String -> IO TestResult
runTest (Test _ source _ _ )  = do
    result <- tryWithTimeout "Interpreter timeout" 3  (return (testSource source))
    case result of
        Right ast -> return $ Right $ showAst ast
        Left err -> return $ Left err
    where testSource :: String -> Either String (PoneProgram (Type Kind))
          testSource source = parsePone source
          showAst :: Show t => PoneProgram t -> String
          showAst ast = show ast

getFiles :: TestSpec -> IO [String]
getFiles (TestSpec t root All) = do
    files :: [String] <- (liftM . filter) isFile $ liftM reverse $ getDirectoryContents root
    return $ map (root ++) files


getFiles (TestSpec t root (Single i)) = return $ [root ++ (intToFile i)]

printResult :: (TestResult, PoneTest String) -> String
printResult (result, test) = toString $ makeJudgement result test

intToFile :: Int -> String
intToFile i = "test" ++ (take (length s) $ cycle "0") ++ s ++ ".pone"
              where s = show i

runTestsAndPrint :: TestSpec -> IO ()
runTestsAndPrint spec = do
    files <- getFiles spec
    tests <- mapM loadTest files
    results <- mapM runTest tests
    print $ map printResult (zip results tests)
