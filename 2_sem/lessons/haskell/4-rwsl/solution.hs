import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Writer

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

evalR :: Expr -> Reader (M.Map Name Integer) Integer
evalR (Num n) = return n
evalR (Var name) = do
    env <- ask
    case M.lookup name env of
        Just n -> return n
        Nothing -> error $ "Unset variable: " ++ name
evalR (Bin op e1 e2) = do
    val1 <- evalR e1
    val2 <- evalR e2
    case op of
        Add -> return (val1 + val2)
        Mul -> return (val1 * val2)
        Sub -> return (val1 - val2)
        Div -> if val2 == 0
            then error "Division by zero"
            else return (val1 `div` val2)
evalR (Let name e1 e2) = do
    val1 <- evalR e1
    local (M.insert name val1) (evalR e2)

minusLoggedR:: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedR acc [] = do
    tell $ show acc
    return acc
minusLoggedR acc (x:xs) = do
     tell $ "(" ++ show x ++ "-"
     y <- minusLoggedR acc xs
     tell ")"
     return (x - y)
