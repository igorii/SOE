import Data.List

data BinaryTree a = Empty
                  | Node a (BinaryTree a) (BinaryTree a)
                  deriving (Ord, Eq)

-- A function that turns a list into an ordered binary tree
treeFromList        :: (Ord a) => [a] -> BinaryTree a
treeFromList []     = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))    
                             (treeFromList (filter (>x) xs))

instance (Show a) => Show (BinaryTree a) where
    show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
        where 
        treeshow pref Empty = ""
        treeshow pref (Node x Empty Empty) = (pshow pref x)
        treeshow pref (Node x left Empty) = 
            (pshow pref x) ++ "\n" ++ 
            (showSon pref "`--" "   " left)
        treeshow pref (Node x Empty right) = 
            (pshow pref x) ++ "\n" ++ 
            (showSon pref "`--" "   " right)
        treeshow pref (Node x left right) = 
            (pshow pref x) ++ "\n" ++ 
            (showSon pref "|--" "|  " left) ++ "\n" ++
            (showSon pref "`--" "   " right)

        showSon pref before next t = 
            pref ++ before ++ treeshow (pref ++ next) t

        pshow pref x = replace '\n' ("\n" ++ pref) (show x)

        replace c new str = 
            concatMap (change c new) str
            where
                change c new x
                    | x == c    = new
                    | otherwise = x:[]


