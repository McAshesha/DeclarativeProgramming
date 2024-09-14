-- 1 утраивание аргумента
f1 :: Double -> Double
f1 x = x * 3

-- 2.1 y = x^2 `div` (1 + x)
f2 :: Double -> Double
f2 x = x^2 / (1 + x)

-- 2.2 y = sqrt (3 * x - x^3)
f3 :: Double -> Double
f3 x = sqrt (3 * x - x^3)

-- 2.3 y = log (x^2 - 21)
f4 :: Double -> Double
f4 x = log (x^2 - 21)

-- 3 функция sign с условным выражением
sign :: Int -> Int
sign x = if x == 0 then 0 else if x < 0 then -1 else 1

-- 4 функция сопоставления с образцом
greet :: String -> String -> String
greet "Finland" name = "Hei, " ++ name
greet "Italy" name = "Ciao, " ++ name
greet "England" name = "How do you do, " ++ name
greet "Russia" name  = "ZOV GOYDA " ++ name
greet _ name = "Hello, " ++ name

-- 5 функция sign охранные выражения
sign2 :: Int -> Int
sign2 x | x == 0 = 0
    | x < 0 = -1
    | otherwise = 1
