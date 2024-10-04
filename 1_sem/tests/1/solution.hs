-- 1 функция степени числа
power :: Int -> Int -> Int
power _ 0 = 1
power x n = x * power x (n - 1)

-- 2 функция sin(x) по Тейлору
sin' :: Double -> Int -> Double
sin' _ n | n < 0 = error "n cannot be negative!"
sin' x 0 = x
sin' x n = part x n + sin' x (n - 1)
    where
        fact n = if n == 0 then 1 else n * fact (n - 1)
        part x n = ((- 1) ^ n) * (x ^ (n * 2 + 1)) / fromIntegral (fact (n * 2 + 1))


-- 3-4 вспомогательная функция для 3 и 4 (можно вставить как локальную в каждую)
isPrime :: Int -> Bool
isPrime m
    | m < 2 = False
    | otherwise = all (\x -> m `mod` x /= 0) [2 .. flSqrt m]
    where flSqrt = floor . sqrt . fromIntegral

-- 3 функция с проблемой гольдбаха
goldbach :: Int -> (Int, Int)
goldbach n
    | n <= 2 || odd n = error "The number must be even and greater than 2!"
    | otherwise = head [(a, n - a) | a <- [2 .. n], isPrime a, isPrime (n - a)]

-- 4 наибольшее простое
biggestPrime :: Int -> Int
biggestPrime n
    | n < 2 = error "Prime number must be greater than 1!"
    | isPrime n = n
    | otherwise = biggestPrime (n - 1)

-- 5 наибольшое значение функции до n
biggestValue :: (Int -> Double) -> Int -> Double
biggestValue f n = maximum [f i | i <- [0 .. n]]

-- 6 определенный интеграл sin
integralSin :: Double -> Double -> Double
integralSin a b = (b - a) * sum [sin (a + (fromIntegral i + 0.5) * (b - a) / 1000) | i <- [0 .. 999]] / 1000

-- 7 определенный интеграл
integral :: (Double -> Double) -> Double -> Double -> Double
integral f a b = (b - a) * sum [f (a + (fromIntegral i + 0.5) * (b - a) / 1000) | i <- [0 .. 999]] / 1000
