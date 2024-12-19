data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Read, Eq)

instance Show a => Show (Tree a) where
  show tree = go tree 0
    where
      go Empty _ = ""
      go (Node val left right) indent =
        replicate (2 * indent) ' ' ++ show val ++ "\n" ++
        go left (indent + 1) ++
        go right (indent + 1)

-- 1. функция симметрии
isSymmetric :: Tree a -> Bool
isSymmetric Empty = True
isSymmetric (Node _ left right) = isMirror left right
    where
        isMirror Empty Empty = True
        isMirror (Node _ l1 r1) (Node _ l2 r2) = isMirror l1 r2 && isMirror r1 l2
        isMirror _ _ = False

-- 2. построение из списка
fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Empty
    where
        insert :: Ord a => a -> Tree a -> Tree a
        insert x Empty = Node x Empty Empty
        insert x (Node node left right)
            | x < node = Node node (insert x left) right
            | x > node = Node node left (insert x right)
            | otherwise = Node node left right

-- 3. удаление минимального
deleteMin :: (Ord a) => Tree a -> (Tree a, a)
deleteMin (Node x Empty right) = (right, x)
deleteMin (Node x left right) = let (newLeft, minVal) = deleteMin left in (Node x newLeft right, minVal)


-- 4. функция для удаления указанного значения
deleteValue :: (Ord a) => a -> Tree a -> Tree a
deleteValue _ Empty = Empty
deleteValue x (Node node left right)
    | x < node = Node node (deleteValue x left) right
    | x > node = Node node left (deleteValue x right)
    | otherwise = case (left, right) of
        (Empty, Empty) -> Empty
        (left, Empty) -> left
        (Empty, right) -> right
        (left, right) -> let (newRight, minVal) = deleteMin right in Node minVal left newRight


