module Morse
  (Morse,
   charToMorse,
   stringToMorse,
   morseToChar,
   morseToString,
   morseCodes
  ) where

import qualified Data.Map as M
import Data.Char (toLower)

-- Тип Morse представлен строкой, содержащей код символа.
type Morse = String

-- Таблица соответствия символ → код Морзе (для букв в нижнем регистре и цифр).
morseCodes :: M.Map Char Morse
morseCodes = M.fromList [
    ('a', ".-"),    ('b', "-..."),  ('c', "-.-."),  ('d', "-.."),
    ('e', "."),     ('f', "..-."),  ('g', "--."),   ('h', "...."),
    ('i', ".."),    ('j', ".---"),  ('k', "-.-"),   ('l', ".-.."),
    ('m', "--"),    ('n', "-."),    ('o', "---"),   ('p', ".--."),
    ('q', "--.-"),  ('r', ".-."),   ('s', "..."),   ('t', "-"),
    ('u', "..-"),   ('v', "...-"),  ('w', ".--"),   ('x', "-..-"),
    ('y', "-.--"),  ('z', "--.."),  ('1', ".----"), ('2', "..---"),
    ('3', "...--"), ('4', "....-"), ('5', "....."), ('6', "-...."),
    ('7', "--..."), ('8', "---.."), ('9', "----."), ('0', "-----")
  ]

-- Обратная таблица для обратного преобразования
reverseMorseCodes :: M.Map Morse Char
reverseMorseCodes = M.fromList $ map (\(k, v) -> (v, k)) (M.assocs morseCodes)

-- Преобразование символа в код Морзе
charToMorse :: Char -> Maybe Morse
charToMorse c = M.lookup (toLower c) morseCodes

-- Преобразование строки в список кодов Морзе
-- Если для какого-либо символа преобразование невозможно, возвращается Nothing
stringToMorse :: String -> Maybe [Morse]
stringToMorse = traverse charToMorse

-- Преобразование кода Морзе в символ
morseToChar :: Morse -> Maybe Char
morseToChar = flip M.lookup reverseMorseCodes

-- Преобразование списка кодов Морзе в строку
morseToString :: [Morse] -> Maybe String
morseToString = traverse morseToChar
