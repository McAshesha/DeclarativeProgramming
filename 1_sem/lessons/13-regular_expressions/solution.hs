import Text.Regex.Posix

isCorrectHaskellFile :: String -> Bool
isCorrectHaskellFile filename = filename =~ "\\.hs$"

isGreeting :: String -> Bool
isGreeting input = input =~ "^([Hh][Ii]).*"

isBinary :: String -> Bool
isBinary input = input =~ "^[0,1]+$"

isPhoneNumber :: String -> Bool
isPhoneNumber input = input =~ "\\+7-[[:digit:]]{3}-[[:digit:]]{3}-[[:digit:]]{4}$"

-- isStartEqualEnd :: String -> Bool
-- isStartEqualEnd input = let (f, s) = (input =~ "^"
