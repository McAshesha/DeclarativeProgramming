-- 1 бесконечный список пифагоровых троек
triples :: [(Int, Int, Int)]
triples = [(a, b, c) | c <- [3..], a <- [1..c], b <- [a..c], a^2 + b^2 == c^2]

-- 2 функция для последовательности по гипотезе Коллатца
collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
  | even n    = n : collatz (n `div` 2)
  | otherwise = n : collatz (3 * n + 1)

-- 3 реализация функции permutations
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [y:zs | y <- xs, zs <- permutations (remove y xs)]
  where
    remove _ [] = []
    remove x (y:ys) | x == y    = ys
                    | otherwise = y : remove x ys

-- 4 реализация функции subsequences
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = subseqWithX (subsequences xs)
  where
    subseqWithX [] = []
    subseqWithX (ys:rest) = ys : (x:ys) : subseqWithX rest

