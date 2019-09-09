import qualified Data.Foldable as F

data Tree a = Empty | Node a (Tree a) (Tree a)
instance (Show a) => Show (Tree a) where
    show Empty = "_"
    show (Node x left right) = show x ++ "(" ++ show left ++ show right ++ ")"
instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)
instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend` f x `mappend` F.foldMap f r

singleton :: a -> Tree a
singleton x = Node x Empty Empty
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Empty = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right -- Node a left rightだとxを使ってなくて怒られる
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

toTree :: (Ord a) => [a] -> Tree a
toTree = foldr treeInsert Empty
fromTree :: Tree a -> [a]
fromTree = F.foldMap (\x -> [x])