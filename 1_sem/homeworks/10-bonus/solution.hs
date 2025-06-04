import Text.Regex.PCRE
import System.IO
import Control.Monad

-- Регулярное выражение для поиска слов в форме {{...{word}...}}
regexPattern :: String
regexPattern = "[^{}]({([^{}]+|(?1))})[^{}]"

-- Функция для извлечения подходящих слов
extractWords :: String -> [String]
extractWords content = getAllTextMatches (content =~ regexPattern :: AllTextMatches [] String)

main :: IO ()
main = do
    -- Открываем файл для чтения
    let fileName = "text.txt"
    content <- readFile fileName

    -- Ищем подходящие слова
    let matches = extractWords content

    -- Выводим результаты
    if null matches
        then putStrLn "Error"
        else do
            putStrLn "Result words:"
            mapM_ putStrLn matches