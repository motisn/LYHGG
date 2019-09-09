import Control.Monad
import Control.Monad.Writer
import Control.Monad.Error
type Birds = Int
type Pole = (Birds, Birds)


{-- Proto type
x -: f = f x
landLeft :: Birds -> Pole -> Pole
landLeft n (left, right) = (left + n, right)
landRight :: Birds -> Pole -> Pole
landRight n (left, right) = (left, right + n)
--}

{-- Maybe type
landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs(left + n - right) < 4 = Just (if left + n > 0 then left + n else 0, right)
    | otherwise = Nothing
landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right) = do
    (left', right') <- Just (left, if right + n > 0 then right + n else 0)
    guard (abs(left' - right') < 4)
    return (left', right')
banana :: Pole -> Maybe Pole
banana _ = Nothing

routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start
    _ <- Nothing
    second <- landRight 2 first
    landLeft 1 second
--}
{-- Writer type (Give up...)
landLeft :: Birds -> Pole -> Writer [String] (Maybe Pole)
landLeft n (left, right)
    | abs(left + n - right) < 4 = do
        if left + n > 0
            then writer (Just (left + n , right), ["OK. " ++ show (left + n, right)])
            else writer (Just (0, right), ["Bird never negative. " ++ show (0, right)])
    | otherwise = do
        tell ["Fall! " ++ show (left + n, right)]
        return Nothing
--}
--Either type
landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right)
    | abs(left + n - right) < 4 = Right (if left + n > 0 then left + n else 0, right)
    | otherwise = Left "(left + n, right)"
landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right)
    | abs(left -n - right) < 4 = Right (left, if right + n > 0 then right + n else 0)
    | otherwise = Left "(left, right + n)"
banana :: Pole -> Either String Pole
banana _ = Left "Banana!"

routine :: Either String Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start
    banana first
    second <- landRight 2 first
    landLeft 1 second