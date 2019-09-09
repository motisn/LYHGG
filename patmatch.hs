-- パターンマッチ
lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky unlucky = "Sorry, you're out of luck, pal!"
-- lucky _ = "hahaha" -- こうすると怒られない。
-- ガード
-- リスト内包表記なら[x*10+3 | (x,3) <- xs]
list103 :: (Num a, Eq a) => [(a, a)] -> [a]
list103 [] = []
--list103 ((x1, x2):xs)
--    | (x2 == 3) = (100 * x1 + 3):(list103 xs)
--    | otherwise = list103 xs
list103 ((x1, 3):xs) = (100 * x1 + 3):(list103 xs)
list103 (_:xs) = list103 xs
-- リスト
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x
last' :: [a] -> a
last' [] = error "Can't call head on an empty list, dummy!"
last' (x:[]) = x
last' (_:x) = last x
-- asパターン
fstlstLetter :: String -> String
fstlstLetter "" = "Empty string, whoops!"
fstlstLetter (x:[]) = "Just a letter, " ++ [x] 
fstlstLetter all@(x:_:y) = "The first letter of " ++ all ++ " is " ++ [x] ++ ", the last is " ++ [last' y]

-- where節
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname
describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
    where what [] = "emply."
          what [x] = "a singleton list."
          what xs = "a longer list."

-- let式
initials' :: String -> String -> String
initials' firstname lastname = 
    let (f:_) = firstname; (l:_) = lastname
    in [f] ++ ". " ++ [l] ++ "."
calcFatBmis :: [(Double, Double)] -> [Double]
calcFatBmis xs = [bmi | (weight, height) <- xs, let bmi = weight / height ^ 2, bmi > 25.0]
describeList'' :: [a] -> String
describeList'' ls = "The list is "
    ++ case ls of [] -> "emply."
                  [x] -> "a singleton list."
                  xs -> "a longer list."

--case式
describeList :: [a] -> String
describeList ls = "The list is "
    ++ case ls of [] -> "emply."
                  [x] -> "a singleton list."
                  xs -> "a longer list."