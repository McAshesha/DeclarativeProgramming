-- 0 факториал с проверкой на отрицательные
factorial :: Int -> Int
factorial n | n < 0 = error "Negative n!"
factorial 1 = 1
factorial n = n * factorial (n - 1)

-- 1 двойной факториал
doubleFactorial :: Int -> Int
doubleFactorial n
    | n < 0 = error "Negative n!"
    | n == 0 || n == 1 = 1
doubleFactorial 2 = 2
doubleFactorial n = n * doubleFactorial (n - 2)

-- 2 функция Аккермана
ak :: Int -> Int -> Int
ak m n
    | m == 0 = n + 1
    | m > 0 && n == 0 = ak (m - 1) 1
    | m > 0 && n > 0 = ak (m - 1) (ak m (n - 1))

-- 3 фибоначи с отрицательными
fib :: Int -> Int
fib n | n < 0 = (-1) ^ (n * (-1) + 1) * fib ((-1) * n)
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- 4 сумма цифр числа
sumCifr :: Int -> Int
sumCifr 0 = 0
sumCifr n = n `mod` 10 + sumCifr (n `div` 10)

-- 5 переворот числа
reverseNum :: Int -> Int
reverseNum n | n <= 9 = n
reverseNum n = (n `div` (10 ^ (countCifr n - 1))) + reverseNum (n `mod` (10 ^ (countCifr n - 1))) * 10
    where
        countCifr :: Int -> Int
        countCifr 0 = 0
        countCifr n = 1 + countCifr (n `div` 10)

-- 6 обратный отчет ракеты
countdown :: Int -> String
countdown n = "Ready! " ++ cdwn n ++ "Liftoff!"
    where
        cdwn 0 = ""
        cdwn n = show n ++ ".. " ++ cdwn (n - 1)

-- 7* количество пересечений
count :: Int -> Int
count 0 = 0
count 1 = 0
count n = n * (n - 1) `div` 2
