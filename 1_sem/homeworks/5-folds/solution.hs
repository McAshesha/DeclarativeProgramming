-- 1. аналоги библиотечных функций
or' :: [Bool] -> Bool
or' = foldl (||) False

all' :: (a -> Bool) -> [a] -> Bool
all' f = foldl (\x y -> f y && x) True

map' :: (a -> b) -> [a] -> [b]
map' f = foldl (\x y -> f y:x) []

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = foldl (\x y -> f y ++ x) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x y -> if f x then x:y else y) []

patrition' :: (a -> Bool) -> [a] -> ([a], [a])
patrition' f = foldr (\x (y, z) -> if f x then (x:y, z) else (y, x:z)) ([], [])

unzip' :: [(a, b)] -> ([a], [b])
unzip' = foldr (\(x, y) (z, w) -> (x:z, y:w)) ([], [])

null' :: [a] -> Bool
null' = foldl (\_ _ -> False) False -- or just: False

intersperse':: a -> [a] -> [a]
intersperse' x (y:ys) = foldl (\z w -> w:x:z) [y] ys

group':: Eq a => [a] -> [[a]]
group' = foldr f []
    where
        f x [] = [[x]]
        f x ((y:ys):yss) | x == y = (x:y:ys):yss
            | otherwise = [x]:((y:ys):yss)


-- 2. интерполяционный многочлен Лагранжа
lagrange:: [(Double, Double)] -> (Double -> Double)
lagrange x = foldr (\(a, b) f c -> f c + b * (\n -> foldr (\(d, e) q -> if d == n then q else \l -> q l * ( (l - d) / (n - d) )) (const 1.0) x) a c) (const 0.0) x

-- Функция для вычисления интерполяционного многочлена Лагранжа
lagrange' :: [(Double, Double)] -> (Double -> Double)
lagrange' points = foldr lagrangeTerm (const 0.0) points
  where
    -- lagrangeTerm создает отдельный член многочлена Лагранжа для каждой точки (a, b)
    lagrangeTerm :: (Double, Double) -> (Double -> Double) -> (Double -> Double)
    lagrangeTerm (a, b) accFunction x = accFunction x + b * basisPolynomial a x

    -- basisPolynomial вычисляет базисный полином l_i(x) для точки a
    basisPolynomial :: Double -> Double -> Double
    basisPolynomial a x = foldr (basisTerm a) (const 1.0) points -- not working

    -- basisTerm вычисляет отдельный множитель (x - x_j) / (x_i - x_j)
    -- для полинома l_i(x), игнорируя точку, где i == j
    basisTerm :: Double -> (Double, Double) -> (Double -> Double) -> (Double -> Double)
    basisTerm a (d, _) accPoly
      | a == d    = accPoly  -- Игнорируем, если точка совпадает с текущей (i == j)
      | otherwise = \x -> accPoly x * ((x - d) / (a - d))


-- 3. функции f и g

-- 1) да

-- 2) [1]

-- 3) [0]

-- 4) a = -c
