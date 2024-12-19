data DayOfWeek = Mon | Tue | Wen | Thu | Fri | Sat | Sun
    deriving Eq

isWeekend :: DayOfWeek -> Bool
isWeekend day = day == Sat || day == Sun

data PointD = PointD Double Double
    deriving Show

distanceToOrigin :: PointD -> Double
distanceToOrigin (PointD x y) = sqrt (x^2 + y^2)

data Shape = Circle Double | Rectangle Double Double | Triangle Double Double Double
    deriving Show

area :: Shape -> Double
area (Circle r) = pi * r^2
area (Rectangle a b) = a * b
area (Triangle a b c) = let pp = perimeter (Triangle a b c) / 2 in sqrt (pp * (pp - a) * (pp - b) * (pp - c))

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle a b) = 2 * (a + b)
perimeter (Triangle a b c) = a + b + c

isSquare :: Shape -> Bool
isSquare (Circle _) = False
isSquare (Triangle {}) = False
isSquare (Rectangle a b) = a == b

data Point a = Point a a

euclideanDistance :: Point Double -> Point Double -> Double
euclideanDistance (Point a b) (Point c d) = sqrt ((a - c)^2 + (b - d)^2)

data List a = Nil | Cons a (List a)
    deriving Show

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)

data Nat = Zero | Suc Nat
    deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Suc (toNat $ n - 1)

addNats :: Nat -> Nat -> Nat
addNats Zero n = n
addNats (Suc x) y = addNats x (Suc y)

multiplyNats :: Nat -> Nat -> Nat
multiplyNats Zero n = Zero
multiplyNats (Suc x) y = addNats y $ multiplyNats x y

factorialNats :: Nat -> Nat
factorialNats Zero = Suc Zero
factorialNats (Suc x) = multiplyNats (Suc x) (factorialNats x)
