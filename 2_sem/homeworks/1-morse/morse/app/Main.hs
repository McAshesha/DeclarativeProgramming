module Main where


-- Импорт необходимых модулей:
-- Morse: содержит функции для преобразования текста и кода Морзе
-- System.Environment: работа с аргументами командной строки
-- Control.Monad: функция unless для условного выполнения действий
import Morse (Morse, stringToMorse, morseToString)
import System.Environment (getArgs)
import Control.Monad (unless)

-- Главная функция программы
main :: IO ()
main = do
    -- Получение аргументов командной строки
    args <- getArgs
    -- Проверка режима работы: to (в Морзе) или from (из Морзе)
    case args of
        ["to"]   -> handleInput encodeToMorse   -- Режим кодирования
        ["from"] -> handleInput decodeFromMorse -- Режим декодирования
        _        -> putStrLn "Usage: morse-exe [to|from]" -- Некорректные аргументы

-- Обработка ввода в зависимости от выбранного режима
-- Принимает функцию-конвертер (toMorse или fromMorse)
handleInput :: (String -> Maybe String) -> IO ()
handleInput converter = do
    -- Чтение строк до пустой строки
    inputLines <- readInputLines
    -- Обработка каждой строки отдельно
    mapM_ (processInputLine converter) inputLines

-- Рекурсивное чтение строк до пустой строки
readInputLines :: IO [String]
readInputLines = do
    let fun acc = do
            line <- getLine -- Чтение строки
            if line == ""   -- Проверка на пустую строку
                then return (reverse acc) -- Возврат собранных строк (в исправленном порядке)
                else fun (line : acc)     -- Накопление строк в аккумуляторе
    fun []

-- Обработка одной строки конвертером
processInputLine :: (String -> Maybe String) -> String -> IO ()
processInputLine converter line = do
    -- Пропуск пустых строк (если такие есть)
    unless (null line) $ do
        case converter line of
            Just result -> putStrLn result          -- Успешное преобразование
            Nothing     -> putStrLn "Invalid input" -- Некорректные данные

-- Преобразование текста в код Морзе (с пробелами между символами)
encodeToMorse :: String -> Maybe String
encodeToMorse s = unwords <$> stringToMorse s

-- Преобразование кода Морзе в текст
decodeFromMorse :: String -> Maybe String
decodeFromMorse s = morseToString (words s)
