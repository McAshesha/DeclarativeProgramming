import Control.Monad.State

data List a = Nil | Cons a (List a) deriving (Show, Eq)

-- 1.1 List представитель класса типов Functor
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- 1.2 функция append для конкатенации двух списков
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

-- 1.3 ist представитель классов типов Applicative и Monad
instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f fs <*> xs = append (fmap f xs) (fs <*> xs)

instance Monad List where
    return = pure
    Nil >>= _ = Nil
    Cons x xs >>= f = append (f x) (xs >>= f)

-- 1.4 Реализовать рекурсивную функцию replace
replace :: List a -> Int -> (List Int, Int)
replace Nil nextVal = (Nil, nextVal)
replace (Cons _ xs) nextVal = let (restList, newNextVal) = replace xs (nextVal + 1)
                                in (Cons nextVal restList, newNextVal)

-- 1.5 Реализовать replace' с использованием монады State
replace' :: List a -> Int -> State Int (List Int)
replace' Nil _ = return Nil
replace' (Cons _ xs) _ = do
    current <- get
    put (current + 1)
    rest <- replace' xs current
    return (Cons current rest)




data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

-- 2.1 функция колва листьев
leaves :: Tree a -> Int
leaves Empty = 0
leaves (Node _ Empty Empty) = 1
leaves (Node _ left right) = leaves left + leaves right

-- 2.2 функция колва узлов с двумя потомками
fullNodes :: Eq a => Tree a -> Int
fullNodes Empty = 0
fullNodes (Node _ left right)
    | left /= Empty && right /= Empty = 1 + fullNodes left + fullNodes right
    | otherwise = fullNodes left + fullNodes right


-- 5. реализация поиска длины наибольшей "крутой" подпоследовательности
coolSequence :: [Int] -> Int
coolSequence xs = coolHelper xs 0 0
  where
    coolHelper [] _ maxLength = maxLength
    coolHelper (y:ys) sumSoFar maxLength
      | y >= sumSoFar = coolHelper ys (sumSoFar + y) (maxLength + 1)
      | otherwise = coolHelper ys sumSoFar maxLength
