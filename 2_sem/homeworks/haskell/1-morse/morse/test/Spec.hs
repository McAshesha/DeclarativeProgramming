import Test.QuickCheck
import Morse
import qualified Data.Map as M
import Data.Char (toLower)


-- Тест QuichCheck: Преобразование символа в Morse и обратно
prop_char_roundtrip :: Char -> Property
prop_char_roundtrip c = c `elem` M.keys morseCodes ==> (morseToChar =<< charToMorse c) == Just c



-- Тест 1: Заглавные буквы преобразуются корректно
testUpperCase :: IO ()
testUpperCase = do
    putStr "Testing uppercase 'HELLO': "
    assert (stringToMorse "HELLO" == Just ["....", ".", ".-..", ".-..", "---"])

-- Тест 2: Невалидные символы возвращают Nothing
testInvalidChar :: IO ()
testInvalidChar = do
    putStr "Testing invalid char '@': "
    assert (stringToMorse "@" == Nothing)

-- Тест 3: Строки с пробелами (пробелы игнорируются)
testSpacesInString :: IO ()
testSpacesInString = do
    putStr "Testing string with spaces 'h e l l o': "
    assert (stringToMorse "h e l l o" == Nothing)

-- Тест 4: Пустая строка
testEmptyString :: IO ()
testEmptyString = do
    putStr "Testing empty string: "
    assert (stringToMorse "" == Just [])

-- Тест 5: Длинные строки
testLongString :: IO ()
testLongString = do
    putStr "Testing long string 'abcdefghijklmnopqrstuvwxyz0123456789': "
    let expected = map (\c -> morseCodes M.! toLower c) "abcdefghijklmnopqrstuvwxyz0123456789"
    assert (stringToMorse "abcdefghijklmnopqrstuvwxyz0123456789" == Just expected)

-- Тест 6: Невалидные коды Морзе
testInvalidMorse :: IO ()
testInvalidMorse = do
    putStr "Testing invalid morse code '......': "
    assert (morseToString ["......"] == Nothing)

-- Тест 7: Цифры
testNumbers :: IO ()
testNumbers = do
    putStr "Testing numbers '12345': "
    assert (stringToMorse "12345" == Just [".----", "..---", "...--", "....-", "....."])

-- Тест 8: Смешанные символы (буквы + цифры)
testMixedChars :: IO ()
testMixedChars = do
    putStr "Testing mixed 'a1b2': "
    assert (stringToMorse "a1b2" == Just [".-", ".----", "-...", "..---"])

-- Тест 9: Символы вне диапазона (знаки препинания)
testPunctuation :: IO ()
testPunctuation = do
    putStr "Testing punctuation 'hello!': "
    assert (stringToMorse "hello!" == Nothing)

-- Тест 10: Лишние пробелы в коде Морзе
testExtraSpacesInMorse :: IO ()
testExtraSpacesInMorse = do
    putStr "Testing morse with extra spaces '  ....   .  ': "
    assert (morseToString (words "  ....   .  ") == Just "he")




-- Тестовые строки из ПРИМЕРА ЗАДАНИЯ
testStrings :: [(String, [Morse])]
testStrings =
    [("sos", ["...", "---", "..."]),
    ("hello", ["....", ".", ".-..", ".-..", "---"]),
    ("world", [".--", "---", ".-.", ".-..", "-.."])]

-- Тест 11: Проверка примеров
testExamples :: IO ()
testExamples = mapM_ testCase testStrings
    where
        testCase (s, m) = do
            putStr $ "Testing '" ++ s ++ "': "
            assertSpace (stringToMorse s == Just m)
            assert (morseToString m == Just s)


-- Прошел или не прошел тест
assert :: Bool -> IO ()
assert True = putStrLn "OK"
assert False = putStrLn "FAIL"

assertSpace :: Bool -> IO ()
assertSpace True = putStr "OK "
assertSpace False = putStr "FAIL "



main :: IO ()
main = do
    quickCheckWith stdArgs {maxSuccess = 100} prop_char_roundtrip
    testUpperCase
    testInvalidChar
    testSpacesInString
    testEmptyString
    testLongString
    testInvalidMorse
    testNumbers
    testMixedChars
    testPunctuation
    testExtraSpacesInMorse
    testExamples
