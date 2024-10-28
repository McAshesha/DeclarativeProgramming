import Data.List (group)
import Data.Char (toUpper, isUpper, chr, ord)

-- 1. группировка подряд идущих элементов
-- 1.1 рекурсия
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems [x] = [[x]]
groupElems (x1 : x2 : xs) = if x1 == x2 then (x1 : g) : gs else [x1] : g : gs
    where (g:gs) = groupElems (x2:xs)

-- 1.3 takeWhile и dropWhile
groupElems' :: Eq a => [a] -> [[a]]
groupElems' (x:xs) = (x : takeWhile (== x) xs) : groupElems' (dropWhile (== x) xs)

-- 2. сжатие строки [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e’)]
encode :: [Char] -> [(Int, Char)]
encode str = map (\xs -> (length xs, head xs)) (group str)

-- 3. декодирование строки
decode :: [(Int, Char)] -> [Char]
decode xs = foldr1 (++) $ map (uncurry replicate) xs

-- 4. заглавная буква каждого слова
capitalize :: [Char] -> [Char]
capitalize str = unwords $ foldr (\(sym:wrd) end -> (toUpper sym : wrd) : end) [] $ words str

-- 5. удалить все заглавные
delAllUpper :: [Char] -> [Char]
delAllUpper str = unwords $ foldr (\wrd acc -> if isWrdUpper wrd then acc else wrd : acc) [] $ words str
    where isWrdUpper = foldr (\sym acc -> isUpper sym && acc) True


-- 6. шифр цезаря
shifr :: [Char] -> Int -> [Char]
shifr str n = map (shiftChar n) str
    where shiftChar n c = chr $ ord c + n

