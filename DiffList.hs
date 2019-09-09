import Control.Monad.Writer

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }
toDiffList xs = DiffList (xs++)
fromDiffList (DiffList f) = f []
instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))
instance Semigroup (DiffList a) where
    (<>) = mappend

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell $ toDiffList ["0"]
finalCountDown x = do
    finalCountDown (x-1)
    tell $ toDiffList [show x]

newtype DiffList' a = DiffList' { getDiffList' :: [a] -> [a] }
toDiffList' xs = DiffList' (xs++)
fromDiffList' (DiffList' f) = f []
instance Monoid (DiffList' a) where
    mempty = DiffList' (\xs -> [] ++ xs)
    (DiffList' f) `mappend` (DiffList' g) = DiffList' $ fmap f g
instance Semigroup (DiffList' a) where
    (<>) = mappend

finalCountDown' :: Int -> Writer (DiffList' String) ()
finalCountDown' 0 = do
    tell $ toDiffList' ["0"]
finalCountDown' x = do
    finalCountDown' (x-1)
    tell $ toDiffList' [show x]