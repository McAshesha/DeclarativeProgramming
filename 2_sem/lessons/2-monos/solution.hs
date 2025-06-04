import Data.Map (Map, fromList, lookup)

-- птицы и трость
type Birds = Int
type Pole = (Birds, Birds)

-- добавить и убавить птиц слева
landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' n (left, right)
        | abs (left + n - right) <= 3 = Just (left + n, right)
        | otherwise = Nothing

-- добавить и убавить птиц справа
landRight' :: Birds -> Pole -> Maybe Pole
landRight' n (left, right)
        | abs (left - (right + n)) <= 3 = Just (left, right + n)
        | otherwise = Nothing

($>) :: a -> (a -> b) -> b
x $> f = f x
infixl 0 $>

-- пример с тростью
example1 = ((0, 0) $> landLeft' 1) >>= landRight' 4 >>= landLeft' (-1) >>= landRight' (-2)

-- выражение
data Expr = Var
          | Num Integer
          | Add Expr Expr
          | Sub Expr Expr

-- вычилсение выражение при помощи do
eval :: Expr -> Maybe Integer
eval Var = Nothing
eval (Num n) = Just n
eval (Add e1 e2) = do
        n1 <- eval e1
        n2 <- eval e2
        return (n1 + n2)
eval (Sub e1 e2) = do
        n1 <- eval e1
        n2 <- eval e2
        return (n1 - n2)

-- вычилсение выражение при помощи bind
eval' :: Expr -> Maybe Integer
eval' Var = Nothing
eval' (Num n) = Just n
eval' (Add e1 e2) = eval e1 >>= \n1 ->
                    eval e2 >>= \n2 ->
                    Just (n1 + n2)
eval' (Sub e1 e2) = eval e1 >>= \n1 ->
                    eval e2 >>= \n2 ->
                    Just (n1 - n2)

-- тип человека
data Person = Person { name :: String, surname :: String }
    deriving (Show, Eq)

-- словарь айди и оценок
grades :: Map Integer Integer
grades = fromList [(124001 , 2), (124002, 4)]

-- словарь фио и айди
studentIds :: Map String Integer
studentIds = fromList [("Ivanov I.", 124001), ("Petrov P.", 124002), ("Sidorova S.", 124003)]

-- парс строки с фио в человека
parse :: String -> Maybe Person
parse s = case words s of
        [name, surname] -> Just (Person name surname)
        _ -> Nothing

-- получение отметки по строке с фио
checkGrade :: String -> Maybe Integer
checkGrade s = do
    person <- parse s
    let fullName = name person ++ " " ++ head (surname person) ++ "."
    studentId <- Data.Map.lookup fullName studentIds
    Data.Map.lookup studentId grades

