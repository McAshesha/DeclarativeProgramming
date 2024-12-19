import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import System.Directory (doesFileExist)
import Control.Exception (catch, IOException)
import Text.Regex.Posix
import Data.Char (isDigit)

-- Type for the phone book
type PhoneBook = Map.Map String String

-- Main program loop
main :: IO ()
main = do
    putStrLn "Phone Book"
    menuLoop Map.empty

-- Menu loop
menuLoop :: PhoneBook -> IO ()
menuLoop phoneBook = do
    putStrLn "\nSelect an action:"
    putStrLn "1. Add contact"
    putStrLn "2. Search contact"
    putStrLn "3. Delete contact"
    putStrLn "4. Save phone book to file"
    putStrLn "5. Load phone book from file"
    putStrLn "6. Exit"
    putStr "Your choice: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> do
            phoneBook' <- addContact phoneBook
            menuLoop phoneBook'
        "2" -> do
            searchContact phoneBook
            menuLoop phoneBook
        "3" -> do
            phoneBook' <- deleteContact phoneBook
            menuLoop phoneBook'
        "4" -> do
            savePhoneBook phoneBook
            menuLoop phoneBook
        "5" -> do
            phoneBook' <- loadPhoneBook
            menuLoop phoneBook'
        "6" -> putStrLn "Exiting the program. Goodbye!"
        _    -> do
            putStrLn "Invalid choice, please try again."
            menuLoop phoneBook

-- Add contact
addContact :: PhoneBook -> IO PhoneBook
addContact phoneBook = do
    putStr "Enter name: "
    hFlush stdout
    name <- getLine
    phone <- getValidPhone
    let formattedPhone = formatPhone phone
    if Map.member name phoneBook
        then do
            putStrLn $ "A contact with the name " ++ name ++ " already exists. Update it? (y/n)"
            hFlush stdout
            confirmation <- getLine
            if confirmation == "y"
                then do
                    let updatedPhoneBook = Map.insert name formattedPhone phoneBook
                    putStrLn "Contact updated."
                    return updatedPhoneBook
                else do
                    putStrLn "Contact not changed."
                    return phoneBook
        else do
            let updatedPhoneBook = Map.insert name formattedPhone phoneBook
            putStrLn "Contact added."
            return updatedPhoneBook

-- Get and validate phone number
getValidPhone :: IO String
getValidPhone = do
    putStr "Enter phone number: "
    hFlush stdout
    phone <- getLine
    if isValidPhone phone
        then return phone
        else do
            putStrLn "Invalid phone number format. Please try again."
            getValidPhone

-- Validate phone number using a regular expression
isValidPhone :: String -> Bool
isValidPhone phone =
    let pattern = "^(\\+7|8)[- ]?(\\([[:digit:]]{3}\\)|[[:digit:]]{3})[- ]?[[:digit:]]{3}[- ]?[[:digit:]]{2}[- ]?[[:digit:]]{2}$" :: String
    in phone =~ pattern

-- Format phone number to a standard format
formatPhone :: String -> String
formatPhone phone =
    let digits = filter isDigit phone
        areaCode = take 3 (drop 1 digits)
        firstPart = take 3 (drop 4 digits)
        secondPart = drop 7 digits
    in "+7-" ++ areaCode ++ "-" ++ firstPart ++ "-" ++ secondPart

-- Search for a contact
searchContact :: PhoneBook -> IO ()
searchContact phoneBook = do
    putStr "Enter name to search: "
    hFlush stdout
    name <- getLine
    case Map.lookup name phoneBook of
        Just phone -> putStrLn $ "Phone number: " ++ phone
        Nothing    -> putStrLn "Contact not found."

-- Delete a contact
deleteContact :: PhoneBook -> IO PhoneBook
deleteContact phoneBook = do
    putStr "Enter name to delete: "
    hFlush stdout
    name <- getLine
    if Map.member name phoneBook
        then do
            let updatedPhoneBook = Map.delete name phoneBook
            putStrLn "Contact deleted."
            return updatedPhoneBook
        else do
            putStrLn "Contact not found."
            return phoneBook

-- Save phone book to a file
savePhoneBook :: PhoneBook -> IO ()
savePhoneBook phoneBook = do
    putStr "Enter file name to save: "
    hFlush stdout
    fileName <- getLine
    catch (writeFile fileName (show phoneBook) >> putStrLn "Phone book saved.")
          handleError
  where
    handleError :: IOException -> IO ()
    handleError _ = putStrLn "Error saving file."

-- Load phone book from a file
loadPhoneBook :: IO PhoneBook
loadPhoneBook = do
    putStr "Enter file name to load: "
    hFlush stdout
    fileName <- getLine
    fileExists <- doesFileExist fileName
    if fileExists
        then do
            content <- catch (readFile fileName) handleError
            case reads content :: [(PhoneBook, String)] of
                [(phoneBook, "")] -> do
                    putStrLn "Phone book loaded."
                    return phoneBook
                _ -> do
                    putStrLn "Error: File content is not a valid phone book."
                    return Map.empty
        else do
            putStrLn "File not found."
            return Map.empty
  where
    handleError :: IOException -> IO String
    handleError _ = do
        putStrLn "Error reading file."
        return ""
