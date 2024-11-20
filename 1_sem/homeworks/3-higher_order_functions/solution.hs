-- 1
combinePredicates :: (Int -> Bool) -> (Int -> Bool) -> (Int -> Bool)
combinePredicates p1 p2 x = p1 x && p2 x

-- 2
differentiate :: (Double -> Double) -> Double -> Double -> Double
differentiate f x h = (f (x + h) - f x) / h

-- 3
solver :: (Double -> Double) -> Double -> Double -> Double -> Double
solver f epsilon a b
    | f a * f b >= 0 = error "The function must have different signs at the boundaries of the interval!"
    | abs (f midpoint) < epsilon = midpoint
    | f midpoint * f a < 0 = solver f epsilon a midpoint
    | otherwise = solver f epsilon midpoint b
    where
        midpoint = (a + b) / 2
