{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell #-}
import System.IO
import Data.Monoid
import Data.Functor
import Data.Tuple.HT (uncurry3)
import qualified Data.Map as Map

import Pone.Parser
import Pone.Interpreter
import Debug.Trace

import Control.Lens

sources = [("C:/Users/M/Desktop/pone/pone_src/test.pone", "second test", 12)] --todo load test results from disk

data PoneTest = Test String String Integer
type TestResult = Either String (Bool, String)

extract :: Show a => (a -> Bool) -> String -> (a -> (Bool, String))
extract f description = (\x -> (f x, description ++ " got " ++ (show x)))

runTest :: PoneTest -> IO (Either String (Bool, String))
runTest (Test filename description expectedValue) = do
    result :: Either String Integer <- testFile filename
    return $ fmap (extract ((==) expectedValue) makeString) result
    where makeString = description ++ ": expected " ++ (show expectedValue)

    
assembleResult :: Either String (Bool, String) -> String
assembleResult (Left error) = "Error: " ++ error
assembleResult (Right (passed, description)) = 
    let passString = (if (passed) then "PASS: " else "FAIL: ") in
    passString ++ description
    
main = let results :: IO [Either String (Bool, String)] = mapM runTest ((map . uncurry3) Test sources) in do
    list :: [Either String (Bool, String)] <- results
    print $ map assembleResult list
       
testFile :: String -> IO (Either String Integer)
testFile filename = do
    source <- readFile filename
    return $ fmap poneEval $ parsePone source


            
newtype Writer w a = Writer { runWriter :: (a, w) }  
instance (Monoid w) => Monad (Writer w) where  
    return x = Writer (x, mempty)  
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

logWriter :: Show a => String -> a -> Writer [String] a
logWriter msg x = Writer (x, [msg ++ show x])
    
addLists :: [a] -> [a] -> [a]
addLists xs ys = xs ++ ys

writerAddLists :: Show a => [a] -> [a] -> Writer [String] [a]
writerAddLists xs ys = do
    xsl :: [a] <- logWriter "heres xs" xs
    ysl :: [a] <- logWriter "heres ys" ys
    return $ xsl ++ ysl
    
    
 -- {-of 
       -- Left err -> trace (show err) Nothing
       -- Right ast -> Just $ poneEval ast-}
       
      
-- checkSource :: (String, Integer) -> IO (Maybe Bool)
-- checkSource (filename,result) = do
    -- evaluated <- testFile filename
    -- return $ do
        -- value <- evaluated
        -- let testResult = value == result 
            -- in return $ testResult
    
