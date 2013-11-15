module Pone.Pretty where

-- | pretty print things. we don't use Show because we don't conform to Read
class Pretty a where
    pretty :: a -> String

instance Pretty a => Pretty ([] a) where
  pretty [] = "[]"
  pretty xs = joinPretty "\n" xs

joinList :: [String] -> String
joinList [] = ""
joinList xs = foldr (\x acc -> x ++  acc) "" xs

join :: String -> [String] -> String
join sep [] = ""
join sep xs = foldr (\x acc -> x ++ sep ++ acc) "" xs

joinPretty :: Pretty a => String -> [a] -> String
joinPretty sep [] = ""
joinPretty sep xs = foldr (\x acc -> (pretty x) ++ sep ++ acc) "" xs