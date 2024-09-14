-- 1.1 y = x^2 / (1 + x)
f1 :: Double -> Double
f1 x = x^2 / (1 + x)

-- 1.2 y = sqrt(3 * x) - x^3
f2 :: Double -> Double
f2 x = sqrt(3 * x) - x^3

-- 1.3 y = log (x^2 - 21)
f3 :: Double -> Double
f3 x = log (x^2 - 21)

-- 1.4 y = logBase 2 (logBase 3 (logBase 4 x))
f4 :: Double -> Double
f4 x = logBase 2 (logBase 3 (logBase 4 x))

-- 1.5 y = sqrt(sin(2 * x)) - sqrt(sin(3 * x))
f5 :: Double -> Double
f5 x = sqrt(sin(2 * x)) - sqrt(sin(3 * x))

-- 2 расчет евклидова~ расстояния
distance :: Double -> Double -> Double -> Double -> Double
distance x1 y1 x2 y2 = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- 3 определение високосного года
leap :: Int -> Bool
leap year = (year `mod` 400 == 0) || (year `mod` 100 /= 0 && year `mod` 4 == 0)