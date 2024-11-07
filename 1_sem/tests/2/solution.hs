import Data.Char (toLower, toUpper)

-- 1. попарно смена мест подряд идущих
pairSwap :: [a] -> [a]
pairSwap [] = []
pairSwap [x] = [x]
pairSwap (x:y:rest) = y : x : pairSwap rest

-- 2. zip с произвольным колвом списков
zipN :: [[a]] -> [[a]]
zipN [] = []
zipN xs
    | any null xs = []
    | otherwise = map head xs : zipN (map tail xs)

-- 3. заглавная буква каждого слова
capitalize :: String -> [String] -> String
capitalize s exceptions = unwords $ map capWord $ words s
    where
        toLowerWord = map toLower
        exceptionsLower = map toLowerWord exceptions
        capWord word
            | toLowerWord word `elem` exceptionsLower = toLowerWord word
            | otherwise = capitalizeWord word
        capitalizeWord (x:xs) = toUpper x : toLowerWord xs
        capitalizeWord [] = []

-- 4. подсписок
sublist :: Eq a => [a] -> [a] -> Bool
sublist [] _ = True
sublist _ [] = False
sublist (x:xs) (y:ys)
    | x == y = sublist xs ys
    | otherwise = sublist (x:xs) ys

-- 5.1 НОД списка значений
gcdl :: (Integral a) => [a] -> a
gcdl = foldr1 gcd

-- 5.2 конкатенация списков
append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs

-- 5.3 элемент в конец списка
snoc :: a -> [a] -> [a]
snoc x = foldr (:) [x]

-- 6. список в множество
toSet :: Eq a => [a] -> [a]
toSet [] = []
toSet (x:xs) = x:toSet (filter (/=x) xs)

-- 7. множество всех подмножеств
power :: Eq a => [a] -> [[a]]
power [] = [[]]
power (x:xs) = let powerXs = power xs in powerXs ++ map (x:) powerXs
