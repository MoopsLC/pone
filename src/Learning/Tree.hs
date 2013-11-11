module Learning.Tree
( Tree(Branch, Empty)
, treeInsert
, treeSingle
, treeFind
) where

data Tree a = Branch a (Tree a) (Tree a) | Empty deriving (Show, Read, Eq) 

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert elm (Branch x l r) 
    | elm < x = Branch x (treeInsert elm l) r
    | elm > x = Branch x l (treeInsert elm r) 
    | elm == x = Branch x l r
    
treeSingle :: a -> Tree a
treeSingle x = Branch x Empty Empty

treeFind :: (Ord a) => a -> Tree a -> Bool
treeFind elm Empty = False
treeFind elm (Branch x l r)
    | elm == x = True
    | elm < x = treeFind elm l 
    | elm > x = treeFind elm r
