import Pone.Test
import Pone.Parser.Type
import Pone.Utils
import System.IO

suite = All

main = do
    hSetEncoding stdout utf8
    results <- runTestsAndPrint (TestSpec 8 "C:/Users/M/Desktop/pone/pone_src/" suite)
    putStrLn $ unlines $ map (++ "\n") results