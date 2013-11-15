module Pone.Pretty where

class Pretty a where
    pretty :: a -> String

instance Pretty a => Pretty ([] a) where
  pretty [] = "[]"
  pretty xs = joinPretty "\n" xs
      --"" ++ (foldr (\x acc -> (pretty x) ++ acc) (pretty x) xs) ++ ""

joinList :: [String] -> String
joinList [] = ""
joinList xs = foldr (\x acc -> x ++  acc) "" xs

join :: String -> [String] -> String
join sep [] = ""
join sep xs = foldr (\x acc -> x ++ sep ++ acc) "" xs

joinPretty :: Pretty a => String -> [a] -> String
joinPretty sep [] = ""
joinPretty sep xs = foldr (\x acc -> (pretty x) ++ sep ++ acc) "" xs