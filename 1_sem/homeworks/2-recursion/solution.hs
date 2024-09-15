-- 1.1 переворот числа
reverseNumber :: Integer -> Integer
reverseNumber n = f n 0
    where
        f 0 rslt = rslt
        f x rslt = f (x `div` 10) (rslt * 10 + x `mod` 10)

-- 1.2 проверка на простоту
isPrime :: Integer -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = f (floor (sqrt (fromIntegral n))) n
    where
        f 1 n = True
        f x n = (n `mod` x) /= 0 && f (x - 1) n

-- 1.3 эффективное фибоначи
fib :: Integer -> Integer
fib n = f n 0 1
  where
    f 0 a _ = a
    f k a b = f (k - 1) b (a + b)

-- 2.1 сумма первых n членов геом прогрессии
sumGeometric :: Float -> Float -> Float -> Float
sumGeometric b1 q 1 = b1
sumGeometric b1 q n
    | n < 1 = error "Not positive n!"
    | otherwise = b1 + q * sumGeometric b1 q (n - 1)

-- 2.2 сумма всех членов геом прогрессии
sumAllGeometric :: Float -> Float -> Float
sumAllGeometric b1 q = if abs q < 1 then b1 / (1 - q) else error "q <= -1 or q >= 1"

-- 2.3 наименьшее n суммы ряда геом прогрессии
getNFromGeometric :: Float -> Float -> Float -> Float
getNFromGeometric b1 q e = f b1 q e 1
    where
        f :: Float -> Float -> Float -> Float -> Float
        f b1 q e n | abs (sumGeometric b1 q n - sumAllGeometric b1 q) < e = n
            | otherwise = f b1 q e (n + 1)
