-- представление двоичного дерева
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

-- пример
tree0 :: Tree Int
tree0 = Node 6
    (Node 4 Empty Empty)
    (Node 3
        (Node 2 Empty Empty)
        (Node 5 Empty Empty))

-- дерево 1
tree1 :: Tree Int
tree1 = Node 3
    (Node 1
        (Node 2 Empty Empty)
        Empty)
    (Node 5
        (Node 4 Empty Empty)
        Empty)

-- дерево 2
tree2 :: Tree Int
tree2 = Node 4
    (Node 2
        (Node 1
            (Node 0 Empty Empty)
             Empty)
        (Node 3 Empty Empty))
    (Node 6
        (Node 5 Empty Empty)
        (Node 7 Empty Empty))

-- значение в корне дерева
valAtRoot :: Tree a -> Maybe a
valAtRoot Empty = Nothing
valAtRoot (Node value _ _) = Just value

-- размер дерева (колво узлов)
treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node _ left right) = 1 + treeSize left + treeSize right

-- сумма значений в узлах
treeSum :: Num a => Tree a -> a
treeSum Empty = 0
treeSum (Node value left right) = value + treeSum left + treeSum right

-- преобразование дерева
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node value left right) = Node (f value) (mapTree f left) (mapTree f right)

-- префиксный обход дерева
preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node value left right) = [value] ++ preorder left ++ preorder right

-- инфиксный обход дерева
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node value left right) = inorder left ++ [value] ++ inorder right

-- постфиксный обход дерева
postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node value left right) = postorder left ++ postorder right ++ [value]

-- выполнение предикатов для всех узлов
allValues :: (a -> Bool) -> Tree a -> Bool
allValues _ Empty = True
allValues condition (Node value left right) =
    condition value && allValues condition left && allValues condition right

-- Поиск элемента в дереве поиска
treeSearch :: (Ord a) => a -> Tree a -> Bool
treeSearch _ Empty = False
treeSearch x (Node value left right)
    | x == value = True
    | x < value = treeSearch x left
    | otherwise = treeSearch x right

