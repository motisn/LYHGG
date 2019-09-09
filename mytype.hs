import qualified Data.Map as Map
import qualified Data.Foldable as F

-- 同一キーをもつMap
testMap = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) [(Nothing, "HAHAHA")
                                                            ,(Just 1, "ONE")
                                                            ,(Just 1, "ICHI")
                                                            ]

-- ロードできない
--data Bit = 0 | 1
--data AB = 'a' | 'b'
--data AB = a | b -- 大文字からじゃないとだめっぽい
-- これはロードできる
--data Bool = False | True
--data AB = A | B
--data BC = B | C -- ABと同時にロードできない

-- リスト
--data List a = Empty | Cons {listhead :: a, listTail :: List a} deriving (Show, Read, Eq Ord)
infixr 5 :>>
data List a = Empty | a :>> (List a) deriving (Show, Read, Eq, Ord)
infixr 5 <<:
(<<:) :: List a -> List a -> List a -- 値コンストラクタでないので(:<<)とはできない。
Empty <<: ys = ys
(x :>> xs) <<: ys = x :>> (xs <<: ys)

-- 二分探索木
data Tree a = EmptyTree | Node a (Tree a) (Tree a) --deriving (Show)
-- かっこよく木を表示したいから手動でShowのインスタンス宣言（かっこよくできてない）
instance (Show a) => Show (Tree a) where
    show EmptyTree = "_"
    show (Node x left right) = "Node: " ++ show x ++ "\nLeft: " ++ show left ++ ", Right: " ++ show right
instance F.Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend` f x `mappend` F.foldMap f r
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right -- Node a left rightだとxを使ってなくて怒られる
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

class YesNo a where
    yesno :: a -> Bool
instance YesNo Int where
    yesno 0 = False
    yesno _ = True
instance YesNo Bool where
    yesno = id
instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

class MyFunctor f where
    fmap :: (a -> b) -> f a -> f b
instance (Ord k) => MyFunctor (Map.Map k) where
    fmap f x = Map.fromList $ map (\(k,v) ->(k,f v)) $ Map.toList x

data TestData = Data1 Int | Data2 {arg :: Int}