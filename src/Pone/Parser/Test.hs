module Pone.Parser.Test where

import Pone.Parser.Type
import Test.QuickCheck

instance Arbitrary TypeRep where
    arbitrary = sized sizedType

sizedType :: Int -> Gen Type
sizedType 0 = genName
sizedType n | n > 0 =
    frequency [ (1, genArrow)
              , (1, genName)
              , (1, genApply)
              ]
  where sub = sizedType (n `div` 4)
        genArrow = ArrowTR <$> ((<::>) <$> sub <*> sub <*> resize 4 (listOf1 (sub :: Gen TypeRep)))
        genApply = ApplyTR <$> ((<::>) <$> sub <*> sub <*> resize 4 (listOf1 (sub :: Gen TypeRep)))
sizedType _ = undefined

arbitraryName :: Gen String
arbitraryName = vectorOf 1 (elements "abcdefgABCDEFG")

genName :: Gen TypeRep
genName = NameTR <$> arbitraryName

stripParens :: String -> String
stripParens = stripOpen . stripClose

stripOpen :: String -> String
stripOpen ('(':xs) = stripOpen xs
stripOpen x = x

stripClose :: String -> String
stripClose x = case reverse x of
    (')':xs) -> stripClose (reverse xs)
    x -> reverse x

myTest :: TypeRep -> Bool
myTest t =
    let lhs :: String = stripParens (parsePoneType $ show t)
        rhs :: String = stripParens (show t)
    in
        {-printInlineStr ("LHS: " ++ lhs ++ " \n" ++ "RHS: " ++ rhs)-} (lhs == rhs)

propTrue :: Gen TypeRep -> Property
propTrue prog = forAll prog myTest

main :: IO ()
main = do
    quickCheck $ propTrue (arbitrary :: Gen TypeRep)