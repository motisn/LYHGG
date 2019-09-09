--{-# OPTIONS -Wall -Werror #-}
-- ↑をつけるとあまりに警告が出たのでやめた。
-- 赤ちゃん関数
doubleMe x = x + x
-- 関数の呼び出し
doubleUs x y = doubleMe x + doubleMe y
-- if式
doubleSmallNumber x = if x > 100
                        then x
                        else x * 2
-- 式なので計算できる
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1
-- 定義、名前、または閉項
conanO'Brien = "It's a-me Conan O'Brien!"

-- 有界でない整数型
factorial :: Integer -> Integer
factorial n = product [1..n]
-- 小数
circumference :: Float -> Float
circumference r = 2 * pi * r
circumference' :: Double -> Double
circumference' r = 2 * pi * r