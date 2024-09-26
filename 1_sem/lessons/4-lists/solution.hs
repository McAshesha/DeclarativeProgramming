-- 0.1 списки
s :: [Int]
s = []

s' :: [Int]
s' = [1]

s'' :: [Int]
s'' = [1, 2]

s''' :: [Int]
s''' = [1, 2]

firstElement :: [a] -> a
firstElement [] = error "Empty list!"
firstElement (x:xs) = x

str = "Wow"
str' = ['W', 'o', 'w']
str'' = ['W', 'o', 'w']

-- 0.2 кортежи
c :: (Int, Char, Double)
c = (1, 'a', 1.2)

-- 1 второй элемент списка
secondElement :: [a] -> a
secondElement [] = error "Empty list!"
secondElement [x] = error "One element in list!"
secondElement (x : y : xs) = y

-- 2 сумма списка
sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs

-- 3 повторение a n раз
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : replicate' (n - 1) a

-- 4 длина списка
length' :: [a] -> Int
length' [] = 0
length' (x : xs) = 1 + length' xs

-- 5 последний элемент списка
last' :: a -> [a] -> a
last' def [] = def
last' def [x] = x
last' def (x : xs) = last' def xs

-- 6 функция map
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x : xs) = f x : map' f xs

-- 7 функция map от двух аргументов
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f xs [] = []
map2 f [] ys = []
map2 f (x : xs) (y : ys) = f x y : map2 f xs ys

-- 8 группировка подряд идущих элементов
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x : xs) = group x [x] xs
  where
    group _ cur [] = [cur]
    group prev cur (y : ys)
      | prev == y  = group prev (y : cur) ys
      | otherwise  = cur : group y [y] ys

