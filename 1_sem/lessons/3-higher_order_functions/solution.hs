-- 0.0
doTwice :: (Int -> Int) -> Int -> Int
doTwice f x = f(f x)

plus1 :: Int -> Int
plus1 x = x + 1

baz = doTwice plus1 7


-- 0.1 функция-генератор функции прибавляющая n
plusn :: Int -> (Int -> Int)
plusn n = f
    where
        f x = x + n

-- 0.2 функция-генератор с let выражением
plusn' :: Int -> (Int -> Int)
plusn' n = let f x = x + n in f

-- 0.3 упрощенная функция-генератор 0.2 и 0.3
plusn'' :: Int -> Int -> Int
plusn'' n x = x + n

-- 1 суммирование целых от a до b
sumInts :: Int -> Int -> Int
sumInts a b = if a < b then a + sumInts (a + 1) b else b

-- 1.1 суммирование целых квадратов от a до b
sumSquares :: Int -> Int -> Int
sumSquares a b = if a < b then a^2 + sumSquares (a + 1) b else b^2

-- 2 функция применяющая функцию на все числа от a до b
higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
higherOrderSum f a b | a > b = 0
                     | a == b = f b
                     | otherwise = f a + higherOrderSum f (a + 1) b

-- 3.1 заданая операция на целых от a до b
operationInts :: (Int -> Int -> Int) -> Int -> Int -> Int
operationInts f a b =  if a < b then f a (operationInts f (a + 1) b) else b

-- 3.2 факториал из заданой операции
factorial :: Int -> Int
factorial n | n <= 0 = error "Not Positive n!"
factorial n = operationInts (*) 1 n
