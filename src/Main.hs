import Pone.Test
import Pone.Parser.Type
import Pone.Utils
import System.IO

suite = Single 1

main = do
    results <- runTestsAndPrint (TestSpec 8 "C:/Users/M/Desktop/pone/pone_src/" suite)
    hSetEncoding stdout utf8
    putStrLn $ unlines $ map (++ "\n") results