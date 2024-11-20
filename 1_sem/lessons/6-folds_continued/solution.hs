doubleAll :: Num a => [a] -> [a]
doubleAll xs = foldr (\a b -> [a * 2] ++ b) [] xs

doubleAll' :: Num a => [a] -> [a]
doubleAll' [] = []
doubleAll' (x:xs) = (x * 2) : doubleAll' xs

doubleAll'' :: Num a => [a] -> [a]
doubleAll'' xs = [x * 2 | x <- xs]

doubleAll''' :: Num a => [a] -> [a]
doubleAll''' = map (* 2)

length' :: [a] -> Int
length' = sum . map (\_ -> 1) -- or sum . map (const 1)

reverse' :: [a] -> [a]
reverse' = foldl (\ys y -> [y] ++ ys) []

last' :: [a] -> a
last' = foldl1 (\_ y -> y)

prefixies :: [a] -> [[a]]
prefixies = foldr (\x acc -> [] : map (x:) acc) [[]]
