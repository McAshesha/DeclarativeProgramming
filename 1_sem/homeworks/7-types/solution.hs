-- 1.1.1 Тип целочисленного выражения
data IntExpr = Num Integer |
    Add IntExpr IntExpr |
    Sub IntExpr IntExpr

instance Show IntExpr where
    show (Num n) = show n
    show (Add ex1 ex2) = "(" ++ show ex1 ++ " + " ++ show ex2 ++ ")"
    show (Sub ex1 ex2) = "(" ++ show ex1 ++ " - " ++ show ex2 ++ ")"

-- 1.1.2 Тип логического выражения
data BoolExpr = Boolean Bool |
    Not BoolExpr |
    And BoolExpr BoolExpr |
    Or BoolExpr BoolExpr |
    Eq IntExpr IntExpr |
    Gt IntExpr IntExpr

instance Show BoolExpr where
    show (Boolean bool) = show bool
    show (Not exp) = "(Not " ++ show exp ++ ")"
    show (And ex1 ex2) = "(" ++ show ex1 ++ " & " ++ show ex2 ++ ")"
    show (Or ex1 ex2) = "(" ++ show ex1 ++ " | " ++ show ex2 ++ ")"
    show (Eq ex1 ex2) = "(" ++ show ex1 ++ " == " ++ show ex2 ++ ")"
    show (Gt ex1 ex2) = "(" ++ show ex1 ++ " > " ++ show ex2 ++ ")"

-- 1.2.1 фукнция для вычисления целочисленного выражения
evalInt :: IntExpr -> Integer
evalInt (Num n) = n
evalInt (Add a b) = evalInt a + evalInt b
evalInt (Sub a b) = evalInt a - evalInt b

-- 1.2.2 фукнция для вычисления логического выражения
boolEval :: BoolExpr -> Bool
boolEval (Boolean b) = b
boolEval (Not e) = not (boolEval e)
boolEval (And a b) = boolEval a && boolEval b
boolEval (Or a b) = boolEval a || boolEval b
boolEval (Eq a b) = evalInt a == evalInt b
boolEval (Gt a b) = evalInt a > evalInt b



-- 2.0 Представление двоичных чисел
data Bin = End | O Bin | I Bin

inc :: Bin -> Bin
inc End = I End
inc (O b) = I b
inc (I b) = O (inc b)

-- 2.1 преобразование двоичного числа в Int
fromBin :: Bin -> Int
fromBin End = 0
fromBin (O b) = 2 * fromBin b
fromBin (I b) = 1 + 2 * fromBin b

-- 2.2 преобразование Int в двоичное число
toBin :: Int -> Bin
toBin 0 = O End
toBin 1 = I End
toBin n | even n = O (toBin (n `div` 2))
        | otherwise = I (toBin (n `div` 2))

-- 2.3 сложение двух двоичных чисел
pls :: Bin -> Bin -> Bin
pls End b = b
pls a End = a
pls (O a) (O b) = O (pls a b)
pls (I a) (O b) = I (pls a b)
pls (O a) (I b) = I (pls a b)
pls (I a) (I b) = O (pls (pls a b) (I End)) -- Перенос разряда

-- 2.4 умножение двух двоичных чисел
mlt :: Bin -> Bin -> Bin
mlt End _ = End
mlt _ End = End
mlt (O a) b = O (mlt a b)
mlt (I a) b = pls b (O (mlt a b))

-- сравнение двоичных чисел
instance Eq Bin where
    End == End = True
    End == O b = b == End
    O a == End = a == End
    O a == O b = a == b
    I a == I b = a == b
    _ == _ = False

-- строковое представление двоичных
instance Show Bin where
    show End = "0"
    show bin = removeLeadingZeros True $ reverse $ toStr bin
      where
        toStr End = ""
        toStr (O b) = '0' : toStr b
        toStr (I b) = '1' : toStr b
        removeLeadingZeros _ "" = ""
        removeLeadingZeros _ "0" = "0"
        removeLeadingZeros _ (x:xs) | x == '1' = x : removeLeadingZeros False xs
        removeLeadingZeros True (x:xs) = removeLeadingZeros True xs
        removeLeadingZeros False (x:xs) = x : removeLeadingZeros False xs
