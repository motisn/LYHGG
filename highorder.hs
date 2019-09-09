-- セクション
deg2rad :: (Floating a) => a -> a
deg2rad = (* (pi / 180))

-- 条件付きmap (mapとfilterあわせたみたいな)
condmap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
condmap _ _ [] = []
condmap p f (x:xs)
    | p x = f x : condmap p f xs
    | otherwise = condmap p f xs

-- 畳み込み
product' :: (Num a) => [a] -> a
product' = foldl (*) 1
mappls3 :: (Num a) => [a] -> [a]
--mappls3 = foldr (\x acc -> (x + 3) : acc) []
--mappls3 = map (+ 3)
mappls3 = map ((+3)$)

-- 関数適用
10puzzle :: (Num a) => [a] -> a


-- キャストのやりかたわからない（これはコンパイルすら通らん）
--fromIntegral' :: (Integral a, Num b) => a -> b
--fromIntegral' x = x