import Data.Char (toLower, isLower, isUpper, chr, ord)
import Data.List (nub, sort, transpose)

-- 1.0 сдвиг символа по цезарю
shift :: Char -> Int -> Char
shift sym n
    | isUpper sym = chr ((ord sym - ord 'A' + n) `mod` 26 + ord 'A')
    | isLower sym = chr ((ord sym - ord 'a' + n) `mod` 26 + ord 'a')
    | otherwise = sym

-- 1.1 шифрование строки
caesarEncrypt :: String -> Int -> String
caesarEncrypt str n = map (`shift` n) str

-- 1.2 дешифрование строки
caesarDecrypt :: String -> Int -> String
caesarDecrypt str n = map (`shift` (-n)) str

-- 1.3 взлом шифра
caesarCrack :: String -> [String]
caesarCrack str = [caesarDecrypt str n | n <- [1..25]]


-- 2.1 проверка строки на уникальность символов
isUnique :: String -> Bool
isUnique str = length str == length (nub str)

-- 2.2 удаление гласных букв
removeVowels :: String -> String
removeVowels = filter isNotVowelSym
    where isNotVowelSym = (`notElem` "AEIOUaeiou")

-- 2.3 проверка на анаграммы
areAnagrams :: String -> String -> Bool
areAnagrams str1 str2
    | lwrStr1 == lwrStr2 = False
    | otherwise = sort lwrStr1 == sort lwrStr2
    where
        lwrStr1 = map toLower str1
        lwrStr2 = map toLower str2

-- 2.4 поиск префикса
longestCommonPrefix :: [String] -> String
longestCommonPrefix [] = ""
longestCommonPrefix [x] = x
longestCommonPrefix = foldr1 commonPrefix
    where
        commonPrefix [] _ = []
        commonPrefix _ [] = []
        commonPrefix (x:xs) (y:ys)
          | x == y = x : commonPrefix xs ys
          | otherwise = []


-- 2.5 список анаграмм
anagramsFor :: String -> [String] -> [String]
anagramsFor word = filter (areAnagrams word)

-- 2.7 разбиение по разделителю
mySplit :: Char -> String -> [String]
mySplit sep str = split str ""
    where
        split [] [] = []
        split [] acc = [acc]
        split (x:xs) acc
            | x == sep = if null acc then split xs "" else acc : split xs ""
            | otherwise = split xs (acc ++ [x])

