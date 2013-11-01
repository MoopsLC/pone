module Pone.List where

{- 
    re-implemented a bunch of functions from Data.List for learning
-}

elem' :: Eq a => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> x == y || acc) False ys

notElem' :: Eq a => a -> [a] -> Bool
notElem' a = not . (elem' a)

sum' :: Num a => [a] -> a
sum' = foldl (+) 0

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

max' :: Ord a => [a] -> a
max' = foldl1 (\acc x -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []
        
product' :: Num a => [a] -> a
product' = foldl (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if (f x) then x : acc else acc) []
        
head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs)  = Just x
    
last' :: [a] -> Maybe a
last' [] = Nothing
last' list = let last = foldl1 (\_ x -> x) list in Just last
    
intersperce' :: a -> [a] -> [a]
intersperce' mid [] = []
intersperce' mid (x:[]) = x : []
intersperce' mid (x:xs) = x : mid : intersperce' mid xs
    
flatten' :: [[a]] -> [a]
flatten' [] = []
flatten' (x:xs) = x ++ flatten' xs

intercalcate' :: [a] -> [[a]] -> [a]
intercalcate' mid list = flatten' $ intersperce' mid list
    
and' :: [Bool] -> Bool
and' [] = False
and' list = foldl (&&) False list

or' :: [Bool] -> Bool
or' [] = False
or' list = foldl (||) False list
    
any' :: (a -> Bool) -> [a] -> Bool 
any' f = foldr ((||) . f) False

all' :: (a -> Bool) -> [a] -> Bool
all' f = and' . map f

iterate' :: (a -> a) -> a -> [a]
iterate' f a = a : iterate' (f . f) a

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = []
    
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
    | p x = dropWhile' p xs
    | otherwise = xs
    
span' :: (a -> Bool) -> [a] -> ([a],[a])
span' p xs = (takeWhile p xs, dropWhile p xs)
  
init' :: [a] -> [a]
init' [] = []
init' (x:[]) = []
init' (x:xs) = x : init' xs

tail' :: [a] -> [a]
tail' = drop 1
  
isEmpty' :: [a] -> Bool
isEmpty' [] = True
isEmpty' _ = False
  
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' (x:xs) = [[]] ++ map (x:) (inits' xs)

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' xs = xs: tails' (drop 1 xs)

isInfixOf' :: Eq a => [a] -> [a] -> Bool
isInfixOf' pre list = any' (isPrefixOf' pre) (tails' list)

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' pre list = take (length pre) list == pre

isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' post list = drop (length list - length post) list == post

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' p xs = ((filter p xs), filter (not . p) xs)

find' :: (a -> Bool) -> [a] -> Maybe a
find' _ [] = Nothing
find' p (x:xs)
    | p x = Just x
    | otherwise = find' p xs

{---todo


split' :: (Eq a) => [a] -> a -> [[a]]
split' (x:xs) el = if (x == el)

nub
insert
groupBy

--this is backwards
union :: Eq a => [a] -> [a] -> [a]
union xs [] = xs
union xs (y:ys) = let un = (union xs ys) in if (notElem y xs) then y : un else un

  -}
   
delete' :: Eq a => a -> [a] -> [a]
delete' elt [] = []
delete' elt (x:xs) = if (x == elt) then xs else x : delete' elt xs

diff' :: Eq a => [a] -> [a] -> [a]
diff' source [] = source
diff' source (d:ds) = let deleted = (delete' d source) in diff' deleted ds
