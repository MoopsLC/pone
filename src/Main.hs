import Pone.Testing

main = do
    results <- runTestsAndPrint (TestSpec 3 "C:/Users/M/Desktop/pone/pone_src/" All)
    putStrLn $ unlines results