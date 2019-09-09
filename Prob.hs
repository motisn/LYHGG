import Data.Ratio
--import Control.Monad

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

instance Applicative Prob where
    pure x = Prob [(x, 1%1)]
    (Prob fs) <*> (Prob xs) = Prob $ [(f x, pf * px) | (f, pf) <- fs, (x, px) <- xs]
    -- (<*>) = ap


flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs where
    multAll (Prob innerxs, p) = map (\(x, r) -> (x, p*r)) innerxs

instance Monad Prob where
    return x = Prob [(x, 1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []

joinProb (Prob xs) = 
    Prob $ compress [(x0, foldl (\acc (x, p) -> if x == x0 then acc + p else acc) 0 xs) | (x0, p0) <- xs] where
        compress [] = []
        compress (x:xs) = if x `elem` xs then compress xs else x:(compress xs)