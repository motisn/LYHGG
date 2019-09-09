minimum' :: (Ord a) => [a] -> a
minimum' [] = error "minimum of empty list!"
minimum' [a] = a
minimum' (x:xs) = min x (minimum' xs)

take' :: Int -> [a] -> [a]
--take' n _
--    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take (n-1) xs

repeat' :: a -> [a]
repeat' x = x : repeat' x

replicate' :: Int -> a -> [a]
replicate' n x = take' n (repeat' x)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger