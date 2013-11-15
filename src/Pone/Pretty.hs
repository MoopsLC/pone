module Pone.Pretty where

class Pretty a where
    pretty :: a -> String

instance Pretty a => Pretty ([] a) where
  pretty [] = "[]"
  pretty (x:xs) =
      "[" ++ (foldr (\x acc -> (pretty x) ++ ", " ++ acc) (pretty x) xs) ++ "]"

joinList :: [String] -> String
joinList [] = ""
joinList (x:xs) = foldr (++) x xs

join :: String -> [String] -> String
join sep [] = ""
join sep (x:xs) = foldr (\x acc -> x ++ sep ++ acc) x xs

joinPretty :: Pretty a => String -> [a] -> String
joinPretty sep [] = ""
joinPretty sep (x:xs) = foldr (\x acc -> (pretty x) ++ sep ++ acc) (pretty x) xs