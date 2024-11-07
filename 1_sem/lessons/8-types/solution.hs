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
perimeter (Circle r) = 2 * pi * r2
perimeter (Rectangle a b) = 2 * (a + b)
perimeter (Triangle a b c) = a + b + c

isSquare :: Shape-> Bool
isSquare (Circle _) = False
isSquare (Triangle {}) = False
isSquare (Rectangle a b) = a == b

data Point a = Point a a

euclideanDistance :: Point Double -> Point Double -> Double
euclideanDistance (Point a b) (Point c d) = sqrt ((a - c)^2 + (b - d)^2)
