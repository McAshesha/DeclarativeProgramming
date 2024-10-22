sum' [] = 0
sum' (x:xs) = x + sum' xs

product' [] = 1
product' (x:xs) = x * product' xs

concat' [] = []
concat' (x:xs) = x ++ concat' xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f c [] = c
foldr' f c (x:xs) = x `f` foldr' f c xs

length' :: [a] -> Int
length' xs = foldr (\_ x -> x + 1) 0 xs

sumSquares :: [Integer] -> Integer
sumSquares xs = foldr (\x y -> x^2 + y) 0 xs

sumPosSquares :: [Integer] -> Integer
sumPosSquares xs = foldr help 0 xs
    where help x y = if x > 0 then x^2 + y else y

myMaximum :: [Integer] -> Integer
myMaximum xs = foldr help 0 xs
    where help x y = if x > y then x else y


meanList :: [Integer] -> Double
meanList xs = let (s, l) = (foldr (\x (y, z) -> (y + x, z + 1) ) (0, 0) xs) in (fromIntegral s) / (fromIntegral l)

sumAndMult :: [Integer] -> (Integer, Integer)
sumAndMult xs = (foldr (\x (y, z) -> (y * x, z + x) ) (1, 0) xs)

reverse' :: [a] -> [a]
reverse' xs = foldr (\y ys -> ys ++ [y]) [] xs

last' :: [a] -> a
last' xs = let (h:new) = foldr (\y ys -> ys ++ [y]) [] xs in h

evenOnly :: [a] -> [a]
evenOnly xs = let (len, ar) = foldl help (0, []) xs in ar
    where help (l, ys) y = if even (l + 1) then (l + 1, ys ++ [y]) else (l + 1, ys)
