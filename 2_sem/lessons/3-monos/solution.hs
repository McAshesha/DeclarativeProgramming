headE :: [a] -> Either String a
headE [] = Left "Cant get head of an empty list"
headE (x:_) = Right x

tailE :: [a] -> Either String [a]
tailE [] = Left "Cant get tail"
tailE (_:xs) = Right xs

sumFS :: Num a => [a] -> Either a
sumFS xs = (+) <$> headE xs <*> (headE =<< tailE xs)

import qualified Data.Map as M

data Expr = Num Integer
          | Var Name
          | Bin Op Expr Expr
          | Let Name Expr Expr
          deriving (Show)

data Op = Add | Mul | Sub | Div
          deriving (Show)

type Name = String

data ExprErr = DivisionByZero | UnsetVariable Name
          deriving (Show, Eq)

evalE :: Expr -> M.Map Name Integer -> Either ExprErr Integer
evalE (Num n) _ = Right n
evalE (Var name) env = case M.lookup name env of
    Just val -> Right val
    Nothing  -> Left (UnsetVariable name)
evalE (Bin op e1 e2) env = do
    val1 <- evalE e1 env
    val2 <- evalE e2 env
    case op of
        Add -> Right (val1 + val2)
        Mul -> Right (val1 * val2)
        Sub -> Right (val1 - val2)
        Div -> if val2 == 0
                  then Left DivisionByZero
                  else Right (val1 `div` val2)
evalE (Let name e1 e2) env = do
    val1 <- evalE e1 env
    let newEnv = M.insert name val1 env
    evalE e2 newEnv

