import Pone.Testing

import System.IO

suite = All

main = do
    results <- runTestsAndPrint (TestSpec 3 "C:/Users/M/Desktop/pone/pone_src/" suite)
    hSetEncoding stdout utf8
    putStrLn $ unlines $ map (++ "\n") results