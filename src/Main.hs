import System.Environment
import Pone.Test
import Pone.Parser.Type
import Pone.Utils
import System.IO

suite = All

main = do
    hSetEncoding stdout utf8
    args <- getArgs
    case args of
        (path:rest) -> do
            results <- runTestsAndPrint (TestSpec 8 path suite)
            putStrLn $ unlines $ map (++ "\n") results
        _ -> error "failed to specify path to pone_src/ for testing"
