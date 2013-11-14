--import Pone.Testing
import Pone.TypeParser
import Pone.Utils
import System.IO
import System.IO.Unsafe
import Control.Applicative((<*), (*>), (<*>), (<$>))
import Control.Monad
import Test.QuickCheck
--suite = All


input = [ ": t -> (m -> n) b as"
        , ": * a a as"
        , ": a -> b -> c as"
        , ": a as"
        , ": (a) as"
        , ": (a -> b) -> c as"
        , ": (a -> b) -> (c -> d) as"
        , ": (a b) -> (c d) as"
        , ": (a) -> b as"
        --, ": s as"
        --, ": k t i as"
        --, ": s as"
        --, ": f n v (u -> s -> x -> r -> o) (t -> y -> a) (e -> x -> k -> f) h as"
        --, ": (v -> y k y b -> v q z -> (y -> h -> x -> e -> j -> i) -> (p -> z -> s -> l -> d -> y)) as"
        --, ": (f -> k -> w -> e) (a -> m -> q -> v) (x -> i -> u) (i -> u -> d -> s -> r) (f -> y -> d -> b) y n p f as"
        --, ": i b o r i j (c -> e -> v -> a -> q) p n i r (v -> a -> v -> i) as"
        --, ": (n r o c j n -> (y -> l -> l -> a -> a) -> w d p a p g -> (n -> g -> v -> r -> a) -> x w f v v -> x r z q b) (w b j d q d -> b y n l p g -> k f g d f f -> x -> c k d) z as"
        --, ": y as"
        --, ": (v -> o p n h y -> t -> y b i g i a (r -> y -> d -> o) z f j y h v j) as"
        ]

printResult :: String -> String
printResult s =
    case parsePoneType s of
        Left err -> err
        Right ast -> show ast

instance Arbitrary Program where
    arbitrary = Program <$> (arbitrary :: Gen Type)

instance Arbitrary Type where
    arbitrary = sized sizedType

(<::>) :: a -> a -> [a] -> [a]
(<::>) x y xs = x:y:xs

sizedType 0 = genName
sizedType n | n > 0 =
    frequency [ (1, genArrow)
              , (1, genName)
              , (1, genApply)
              ]
  where sub = sizedType (n `div` 4)
        genArrow = Arrow <$> ((<::>) <$> sub <*> sub <*> resize 4 (listOf1 (sub :: Gen Type)))
        genApply = Apply <$> ((<::>) <$> sub <*> sub <*> resize 4 (listOf1 (sub :: Gen Type)))
sizedType _ = undefined

arbitraryName :: Gen String
arbitraryName = vectorOf 1 (elements "abcdefghijklmnopqrstuvwxyz")

genName :: Gen Type
genName = Name <$> arbitraryName


extractError :: Show a => Either String a -> String
extractError (Left err) = err
extractError (Right s) = show s

checkProgram :: Program -> Bool
checkProgram (Program t) = check t

check :: Type -> Bool
check (Name n) = False
check (Apply xs) = any check xs
check (Arrow xs) = any isApply xs || any check xs

isApply :: Type -> Bool
isApply (Name n) = False
isApply (Apply xs) = True
isApply (Arrow xs) = False

quickTest = do
    let input :: IO [Program] = sample' (arbitrary :: Gen Program)
        strings :: IO [String] = liftM (map ((\x -> ": " ++ x ++ " as") . show)) input
        res :: IO [Either String Program] = liftM (map parsePoneType) strings
        extracted :: IO [String] = liftM (map extractError) res in do
            e <- extracted
            putStrLn $ unlines  $ map (++ "\n") e


--testList :: [String] -> IO String
testList strings = do
    let res :: IO [Either String Program] = liftM (map parsePoneType) strings
        extracted :: IO [String] = liftM (map extractError) res in do
            e <- extracted
            putStrLn $ unlines  $ map (++ "\n") e

--printThenEquals :: String -> String -> Bool
--printThenEquals s t b =

myTest :: Program -> Bool
myTest prog =
    let lhs :: String = (extractError $ parsePoneType $ show prog)
        rhs :: String = show prog
    in
        printInlineStr ("LHS: " ++ lhs ++ " \n" ++ "RHS: " ++ rhs) (lhs == rhs)

propTrue :: Gen Program -> Property
propTrue prog = forAll prog myTest
--main :: IO ()
main = do
    --testList $ return input
    --quickTest
    quickCheck $ propTrue (arbitrary :: Gen Program)
    --let program = Program (Name "m") in
    --    let s :: String = (extractError $ parsePoneType $ show program) in do
    --        print s
    --        print $ show program
            --putStrLn (show $ Program $ Arrow [Apply [(Name "a"), (Name "b")], Apply [(Name "a"), (Name "b")]])
    --results <- runTestsAndPrint (TestSpec 8 "C:/Users/M/Desktop/pone/pone_src/" suite)
    --hSetEncoding stdout utf8
    --putStrLn $ unlines $ map (++ "\n") results