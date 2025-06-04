-----------------------------------------------------------------------------
-- |
-- Module      :  SQLHS
-- Copyright   :  (c) 2019 Konstantin Pugachev
-- License     :  MIT
--
-- Maintainer  :  K.V.Pugachev@inp.nsk.su
-- Stability   :  experimental
-- Portability :  portable
--
-- The SQLHS module provides base SQL operations using Haskell syntax.
-- The tables have operation counter and support laziness if possible.
--
-----------------------------------------------------------------------------

module SQLHS (
  Cell(..), Row, Table, complexityValue, emptyTable, row, table,
  Key, HashedTable,
  debugTable, printResult, runSQLHSExample, runSQLHSLazynessExample,
  fakeRow, isFakeRow,
  
  naiveJoin, crossJoin, tableToIndex, indexToTable, hashJoin, mergeJoin,
  limit, selectStar, orderBy, sortBy, naiveFilter, naiveDistinct,
  project, enumerate,
  
  col, cols
) where

-- package: multimap
-- http://hackage.haskell.org/package/multimap-1.2.1/docs/Data-MultiMap.html

import CountableTable

import Data.List (sortBy, elemIndex)
import Data.Function (on)
import qualified Data.MultiMap as MM
import Control.Applicative (liftA2)
import Control.Monad (forM_)
import Data.Ord (Down)
import Data.Maybe (isJust, fromJust)

type Key = Row
type HashedTable = Complex (MM.MultiMap Key Row)

ilog :: Integral n => n -> Integer
ilog = (+1) . round . log . fromIntegral

ilogSize :: HashedTable -> Integer
ilogSize = ilog . MM.size . value

ilen :: Complex [a] -> Integer
ilen = fromIntegral . length . value

printResult :: Table -> IO ()
printResult t = do
  let ops = complexityValue t
  let rows = filter (not . isFakeRow) (value t)
  let nrows = length rows
  
  putStrLn "Result:"
  forM_ (take 10 rows) print
  putStrLn $ "Rows: " ++ show nrows ++ ". Operations: " ++ show ops

fakeOnFakes :: (Row -> Key) -> (Row -> Key)
fakeOnFakes f r | isFakeRow r = r
                | otherwise   = f r

ensureFake :: [Row] -> [Row]
ensureFake (x:xs) | isFakeRow x = x:xs
                  | otherwise   = x : ensureFake xs
ensureFake []                   = [fakeRow]

-- naivestJoin добавляет fakeRow в середину, поэтому
-- НЕЛЬЗЯ ИСПОЛЬЗОВАТЬ naivestJoin БЕЗ naiveFilter
naivestJoin :: Table -> Table -> Table
-- константно учитывается трудоёмкость создания обеих таблиц
-- + формируем таблицу с ленивыми
naivestJoin tl tr
  -- отсекаем случаи, когда одна из таблиц пустая (но учитываем трудоёмкость их генерации)
  | isEmptyTable tl = tl >> tr >> emptyTable
  | isEmptyTable tr = tl >> tr >> emptyTable
  -- определяем ширину таблиц, выкусываем из соединённых строк - исходные строки
  | otherwise = tl >> tr >> table (nj (ensureFake $ value tl) (ensureFake $ value tr))
  where
  -- для соединения с 1й строкой левой таблицы учитывается сложность создания строк правой таблицы,
  -- для соединений с последующими - не учитывается
  -- TODO: не мапить на каждой итерации
    nj (l:ls) rs = nj' l rs ++ nj ls (map zeroCostRow rs)
    nj [] _ = []
    -- сложность создания строки - доступ к левой, правой (2) + сложности создания левой и правой
    -- (для соединения с последующими строками левой сюда будет передан ноль для правой)
    -- для последующих случаев сложность создания левой приравнимается к нулю
    nj' l (r:rs) = consNotFake l r : nj' (zeroCostRow l) rs
    nj' _ [] = []
    consNotFake x y | isFakeRow x || isFakeRow y = x >> y >> fakeRow
                    | otherwise                  = count 2 >> liftA2 (++) x y

limit :: Int -> Int -> Table -> Table
limit s c t = do
  -- добавляется константно сложность выброшенных строк + по 1 на их проход
  count (fromIntegral s)
  count (sum . map cxty . take s $ value t)
  -- учитываем лениво чтение каждой строки
  fmap (map (count 1 >>) . take c . drop s) t

selectStar :: Table -> Table
-- добавляет каждой строке единицу для чтения
selectStar = mapTable (count 1 >>)

orderBy :: (Row -> Key) -> Table -> Table
orderBy getKey t = do
  count (complexityValue t)
  count (ilen t * ilog (ilen t))
  table . sortBy (compare `on` fakeOnFakes getKey) . value $ zeroCostTable t

naiveDistinct :: Table -> Table
-- nub (x:xs) matched = if x `elem` matched then nub xs matched
--                                          else x : nub xs (x:matched)
-- nub [] matched = []
naiveDistinct t = count (cxty t) >> table (fltr 0 (value t) []) where
  -- учли cxty t отдельно, храним временное значение сложности (вначале 0)
  -- на каждый доступ добавляем 1, на сравнение - 1
  -- если уникально, к сложности строки прибавляется временная сложность - сложности пропущенных строк
  fltr :: Integer -> [Row] -> [Row] -> [Row]
  fltr c (r:rs) matched
    -- если фэйковая, увеличиваем временную сложность на её значение
    | isFakeRow r = fltr (c + cxty r) rs matched
    -- если не уникально, увеличиваем временная сложность плюс цена проверки
    -- (1 доступ + 1 сравнение) * количество пройденных элементов
    | isJust idx  = fltr (c + cxty r + costOfFinding) rs matched
    -- элемент не нашли в списке matched, но старались
    -- (1 доступ + 1 сравнение) * длина matched
    | otherwise = (count (c + costOfMissing) >> r) : fltr 0 rs (r:matched)
    where
      idx = r `elemIndex` matched
      costOfFinding = 2 * (1 + fromIntegral (fromJust idx))
      costOfMissing = 2 * fromIntegral (length matched)
  fltr 0 [] _ = []
  -- если после прохода осталась сложность, добавляем фэйковую строку-терминатор
  fltr c [] _ = [count c >> fakeRow]

naiveFilter :: (Row -> Bool) -> Table -> Table
naiveFilter = variableCostNaiveFilter 2

variableCostNaiveFilter :: Integer -> (Row -> Bool) -> Table -> Table
variableCostNaiveFilter cost f t = count (cxty t) >> table (fltr 0 (value t)) where
  -- учли cxty t отдельно, храним временное значение сложности (вначале 0)
  -- на каждый доступ добавляем 1, на сравнение - 1
  -- если заматчено, к сложности строки прибавляется временная сложность - сложности пропущенных строк
  fltr :: Integer -> [Row] -> [Row]
  fltr c (r:rs)
    -- если фэйковая, увеличиваем временную сложность на её значение
    | isFakeRow r = fltr (c + cxty r) rs
    -- если заматчено, учитываем временную сложность в этой строке
    | f r = (count (c + cost) >> r) : fltr 0 rs
    -- если не заматчено, увеличиваем временную сложность
    | otherwise = fltr (c + cxty r + cost) rs
  fltr 0 [] = []
  -- если после прохода осталась сложность, добавляем фэйковую строку-терминатор
  fltr c [] = [count c >> fakeRow]

naiveJoin :: (Row -> Key) -> (Row -> Key) -> Table -> Table -> Table
naiveJoin fl fr tl tr
  -- отсекаем случаи, когда одна из таблиц пустая (но учитываем трудоёмкость их генерации)
  | isEmptyTable tl = tl >> tr >> emptyTable
  | isEmptyTable tr = tl >> tr >> emptyTable
  -- определяем ширину таблиц, выкусываем из соединённых строк - исходные строки
  | otherwise = naiveFilter f (naivestJoin tl tr)
  where
    lwidth :: Int
    lwidth = length . value . head . value $ tl
    f r = fl' (row rl) == fr' (row rr) where (rl, rr) = splitAt lwidth . value $ r
    fl' = fakeOnFakes fl; fr' = fakeOnFakes fr

crossJoin :: Table -> Table -> Table
-- костыль: filter умеет сбивать фейковые строки в конец, поэтому прогоняем им
crossJoin tl = freeFilter (const True) . naivestJoin tl
  where freeFilter = variableCostNaiveFilter 0

tableToIndex :: (Row -> Key) -> Table -> HashedTable
tableToIndex getKey t = do
  -- сложность вычисления таблицы добавляется константно
  count (cxty t)
  -- count (ilen t * ilog(ilen t)) -- а сам индекс бесплатен
  let rows = ensureFake $ value t
  let keys = map (fakeOnFakes getKey) rows
  return . MM.fromList $ zip keys rows

-- zero cost operation
indexToTable :: HashedTable -> Table
indexToTable = fmap (map snd . MM.toList)

hashJoin :: (Row -> Key) -> Table -> HashedTable -> Table
hashJoin getKey t h
  -- отсекаем случаи, когда одна из таблиц пустая (но учитываем трудоёмкость их генерации)
  | isEmptyTable t         = t >> h >> emptyTable
  | MM.size (value h) == 0 = t >> h >> emptyTable
  -- учитываем накладные расходы на таблицу и сложность создания индекса
  | otherwise              = count (cxty t) >> h >> hj (fmap ensureFake $ zeroCostHeaderTable t)
  where
    hj :: Table -> Table
    hj t = table (value t >>= hj1)
    -- кусок join'а для одной левой строки
    hj1 :: Row -> [Row]
    hj1 r = joined' where
      rights = MM.lookup (fakeOnFakes getKey r) (value h)
      rights'' = map (count 1 >>) rights -- учёт чтения правых строк
      joined = map (liftA2 (++) (zeroCostRow r)) rights'' -- соединение
      cost = 1 + cxty r + ilogSize h -- чтение левой + цена вычисления левой + лукап правых
      joined' = mapHead (count cost >>) joined -- учёт накладных расходов в цене первой строки
    mapHead f (x:xs) = f x : xs
    mapHead _ [] = []

mergeJoin :: (Row -> Key) -> (Row -> Key) -> Table -> Table -> Table
mergeJoin fl fr tl tr = mj prepared_tl prepared_tr where
  prepared_tl = count (cxty tr) >> fmap ensureFake tl
  prepared_tr = zeroCostHeaderTable $ fmap ensureFake tr
  -- сохраняем константную стоимость обеих таблиц как стоимость левой
  -- затем будем каждой первой существующей строке соединения назначать эту стоимость и сбрасывать
  -- при каждом пролистывании строки - увеличивать эту стоимость
  -- в самом конце то, что останется (останется, если конец соединения пуст)
  mj :: Table -> Table -> Table
  mj tl tr
    | isEmptyTable tl || isEmptyTable tr =
      if cxty tl > 0 || cxty tr > 0
      -- если сложность осталась, добавляем фэйковую строку
      then table [count (cxty tl + cxty tr) >> fakeRow]
      else emptyTable
    -- учитывем цену доступа + цену получения пропущенного левого
    | l' <  r' = mj (count (cl+2) >> ls') tr
    -- учитывем цену доступа + цену получения пропущенного правого
    | l' >  r' = mj (count (cr+2) >> tl) rs'
    -- учитываем, что левый столбец будет учтён в lefts, поэтому сбрасываем его цену для следующих итераций
    | l' == r' = liftA2 (++) (lefts tl r) (mj (table (zeroCostRow l : ls)) rs')
    where
      cl = cxty tl; (l:ls) = value tl
      cr = cxty tr; (r:rs) = value tr
      l' = fakeOnFakes fl l; r' = fakeOnFakes fr r
      ls' = table ls; rs' = table rs
      
      -- вписываем цену таблицы в созданную строку
      -- правый столбец даёт стоимость только один раз
      lefts :: Table -> Row -> Table
      lefts (Complex c (l:ls)) r
        | l' == fl l = fmap (newrow:) $ lefts (table ls) (zeroCostRow r)
        | otherwise  = count c >> emptyTable
        where
          newrow = count (2+c) >> liftA2 (++) l r
      lefts tl _ = tl

dec x = x-1

col :: Int -> Row -> Row
col n = row . (:[]) . (!! dec n) . value

cols :: [Int] -> Row -> Row
cols ns r = row . map (value r !!) . map dec $ ns

-- добавляет к каждой строке +1 за чтение
-- если proj r вернула строку со сложностью c,
-- сложность строки результата будет cxty r + c + 1
project :: (Row -> Key) -> Table -> Table
project proj = fmap . map . fakeOnFakes $ projectRow
  where projectRow r = (count (cxty r  + 1) >>) . proj $ r

-- добавляет к каждой строке +1 за чтение
enumerate :: Table -> Table
enumerate = fmap $ zipWith (liftA2 maybeCons) (map (Complex 1 . IntCell) [1..]) where
  maybeCons _ xs@(FakeCell:_) = xs
  maybeCons x xs              = x:xs

--------------------------------------------------------
-- Функции для демонстрации работы. В общем-то, не нужны

test msg t = do
  putStrLn ("===== " ++ msg ++ " =====")
  putStrLn (debugTable t)
  putStrLn ""

runSQLHSLazynessExample = do
  test "[1..], < 10, limit 5" (limit 0 5 $ naiveFilter leq10 $ nats)
  test "[1..], limit 10, limit 5" (limit 0 5 $ limit 0 10 $ nats)
  test "[1..1000], limit 10, limit 5" (limit 0 5 $ limit 0 10 $ nats1000)
  test "[1..1000], < 10, limit 15" (limit 0 15 $ naiveFilter leq10 $ nats1000)
  test "[1..1000], < 10" (naiveFilter leq10 nats1000)
  test "[1..1000], < 10, < 10" (naiveFilter leq10 $ naiveFilter leq10 $ nats1000)
  where
    mod10eq0 (Complex c (IntCell x:_)) = x `mod` 10 == 0
    leq10 (Complex c (IntCell x:_)) = x <= 10
    nats = table . map (row . (:[]) . IntCell) $ [1..]
    nats1000 = table . map (row . (:[]) . IntCell) $ [1..1000]

runSQLHSExample = do
  tests "Let the tables creation cost be 0" t1 t2
  let t1c = count 10000 >> mapTable (count 100 >>) t1
  let t2c = count 100000000 >> mapTable (count 1000000 >>) t2
  tests "Let the tables creation cost be 100 per line + 10000 for t1 and 1000000 per line + 100000000 for t2" t1c t2c
  where 
    t1 = table [
      row [IntCell 30, StringCell "hello"],
      row [IntCell 10, StringCell "world"],
      row [IntCell 10, StringCell "world2"],
      row [IntCell 20, StringCell "xxx"]]
    t2 = table [
      row [IntCell 30, DoubleCell 1.1],
      row [IntCell 0, DoubleCell 0.1],
      row [IntCell 20, DoubleCell 2.2],
      row [IntCell 10, DoubleCell 8.1],
      row [IntCell 20, DoubleCell 5.2]]
    t1s = orderBy (col 1) t1
    t2s = orderBy (col 1) t2
    tests msg t1 t2 = do
      putStrLn msg
      test "t1" t1
      test "t2" t2
      test "t1 naivest join t2" $ naivestJoin t1 t2
      test "t1 cross join t2" $ crossJoin t1 t2
      test "t1 naivest join t2 limit 2 5" $ limit 2 5 $naivestJoin t1 t2
      test "t1 cross join t2 limit 2 5" $ limit 2 5 $ crossJoin t1 t2
      test "t1 order by 1" $ t1s
      test "t2 order by 1" $ t2s
      test "t1 naive join t2 on 1" $ naiveJoin (col 1) (col 1) t1 t2
      test "t1 hash join t2 on 1" $ hashJoin (col 1) t1 $ tableToIndex (col 1) t2
      test "t1 merge join t2 on 1" $ mergeJoin (col 1) (col 1) t1s t2s
      test "t1 naive join t2 on 1 limit 2 2" $ limit 2 2 $ naiveJoin (col 1) (col 1) t1 t2
      test "t1 hash join t2 on 1 limit 2 2" $ limit 2 2 $ hashJoin (col 1) t1 $ tableToIndex (col 1) t2
      test "t1 merge join t2 on 1 limit 2 2" $ limit 2 2 $ mergeJoin (col 1) (col 1) t1s t2s
      test "t1 order by 1 limit 2 2" $ limit 2 2 $ orderBy (col 1) t1

