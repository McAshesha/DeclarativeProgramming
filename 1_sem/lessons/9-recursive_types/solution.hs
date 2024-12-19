data Expr = Var |
            Num Integer |
            Add Expr Expr |
            Sub Expr Expr

eval :: Expr -> Maybe Integer
eval Var = Nothing
eval (Num n) = Just n
eval (Add ex1 ex2) = applyMaybe (eval ex1) (eval ex2) (+)
eval (Sub ex1 ex2) = applyMaybe (eval ex1) (eval ex2) (-)

applyMaybe :: Maybe Integer -> Maybe Integer -> (Integer -> Integer -> Integer) -> Maybe Integer
applyMaybe (Just x) (Just y) f = Just $ f x y
applyMaybe _ _ _ = Nothing

evalVar :: Expr -> Integer -> Integer
evalVar Var x = x
evalVar (Num n) _ = n
evalVar (Add ex1 ex2) x = evalVar ex1 x + evalVar ex2 x
evalVar (Sub ex1 ex2) x = evalVar ex1 x - evalVar ex2 x

instance Show Expr where
    show Var = "x"
    show (Num n) = show n
    show (Add ex1 ex2) = "(" ++ show ex1 ++ " + " ++ show ex2 ++ ")"
    show (Sub ex1 ex2) = "(" ++ show ex1 ++ " - " ++ show ex2 ++ ")"

size :: Expr -> Int
size (Num _) = 0
size (Add ex1 ex2) = 1 + size ex1 + size ex2
size (Sub ex1 ex2) = 1 + size ex1 + size ex2


data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

tree1 = Node 6
            (Node 4 Empty Empty)
            (Node 3
                    (Node 2 Empty Empty)
                    (Node 5 Empty Empty))


tree2 = Node 3
            (Node 1 Empty (Node 2 Empty Empty))
            (Node 5 (Node 4 Empty Empty) Empty)


valAtRoot :: Tree a -> Maybe a
valAtRoot Empty = Nothing
