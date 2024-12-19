import Control.Monad (when)

main = do
    putStrLn "Hello, what is your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ ", nice to meet you!")

printList :: [Int] -> IO ()
printList [] = return ()
printList (x:xs) = do
    print x
    printList xs

produceTwo:: IO Int
produceTwo = do
    return 1
    return 2

readUntil :: (String -> Bool) -> IO [String]
readUntil f = do
    line <- getLine
    if f line
        then
            return []
        else do
            lines <- readUntil f
            return (line : lines)


while :: IO Bool -> IO () -> IO ()
while condop body = do
    cond <- condop  -- Проверяем условие
    when cond $ do
        body
        while condop body

-- notEmptyLine :: String -> IO Bool
-- notEmptyLine line = (line /= "")

-- reverseWords :: String -> String
-- reverseWords = unwords . map reverse . words

-- readAndReverse :: IO ()
-- readAndReverse = do
--     line <- getLine
--     while (notEmptyLine line) do
--         print $ reverseWords line
--         line <- getLine
