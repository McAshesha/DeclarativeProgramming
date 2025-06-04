-----------------------------------------------------------------------------
-- |
-- Module      :  CountableTable
-- Copyright   :  (c) 2019-2021 Konstantin Pugachev
-- License     :  MIT
--
-- Maintainer  :  K.V.Pugachev@inp.nsk.su
-- Stability   :  provisional
-- Portability :  portable
--
-- The CountableTable module provides datatypes for expressing tables
-- with operation counters.
--
-----------------------------------------------------------------------------

module CountableTable (
  Cell(..), Row, Table, Complex(..),
  count, complexityValue, emptyTable, isEmptyTable, row, table, mapTable,
  zeroCostRow, zeroCostTable, zeroCostHeaderTable, debugTable,
  fakeRow, isFakeRow
) where

data Cell = IntCell Int | StringCell String | DoubleCell Double | DescCell Cell | FakeCell
data Complex a = Complex { cxty :: Integer, value :: a }
type Row = Complex [Cell]
type Table = Complex [Row]

isEmptyTable :: Table -> Bool
isEmptyTable = null . value

isFakeRow :: Row -> Bool
isFakeRow (Complex _ (FakeCell:_)) = True
isFakeRow _                        = False

fakeRow :: Row
fakeRow = row [FakeCell]

-- joinFakes :: [Row] -> [Row]
-- joinFakes (x:y:xs)
--   | isFakeRow x && isFakeRow y = joinFakes $ x >> y >> fakeRow : xs
--   | otherwise = x : joinFakes xs
-- joinFakes xs = xs

mapTable :: (Row -> Row) -> Table -> Table
mapTable f = fmap (map f)

complexityValue :: Table -> Integer
complexityValue t = sum (cxty t : map cxty (value t))

instance Show Cell where
  show (IntCell a)    = show a
  show (StringCell a) = a
  show (DoubleCell a) = show a
  show FakeCell       = "FAKE"
  show (DescCell c)   = show c

instance Eq Cell where
  (IntCell a)    == (IntCell b)    = a == b
  (StringCell a) == (StringCell b) = a == b
  (DoubleCell a) == (DoubleCell b) = a == b
  FakeCell       == FakeCell       = True
  FakeCell       == _              = False
  _              == FakeCell       = False
  (DescCell a)   == b              = a == b
  a              == (DescCell b)   = a == b
  a == b = error $ "Cannot compare values of different types: "
    ++ show a ++ " == " ++ show b

instance Ord Cell where
  (IntCell a)    `compare` (IntCell b)    = a `compare` b
  (StringCell a) `compare` (StringCell b) = a `compare` b
  (DoubleCell a) `compare` (DoubleCell b) = a `compare` b
  FakeCell       `compare` FakeCell       = EQ
  _              `compare` FakeCell       = LT
  FakeCell       `compare` _              = GT
  (DescCell a)   `compare` (DescCell b)   = b `compare` a
  a              `compare` (DescCell b)   = error $ "Cannot compare ordinary cell and DescCell: "
    ++ show a ++ " `compare` DescCell " ++ show b
  (DescCell a)   `compare` b   = error $ "Cannot compare ordinary cell and DescCell: DescCell "
    ++ show a ++ " `compare` " ++ show b
  a `compare` b = error $ "Cannot compare values of different types: "
    ++ show a ++ " `compare` " ++ show b

instance Show a => Show (Complex a) where
  show a = show . value $ a

instance Eq a => Eq (Complex a) where
  a == b = (value a) == (value b)

instance Ord a => Ord (Complex a) where
  a `compare` b = (value a) `compare` (value b)

instance Functor Complex where
  fmap f c = Complex (cxty c) (f $ value c)

instance Applicative Complex where
  f <*> c = Complex (cxty f + cxty c) ((value f) (value c))
  pure x = Complex 0 x

instance Monad Complex where
  x >>= f = Complex (cxty x + cxty fx) (value fx) where
    fx = f (value x)
  return x = Complex 0 x

count :: Integer -> Complex ()
count a = Complex a ()

emptyTable :: Table
emptyTable = return []

row :: [Cell] -> Row
row = return

table :: [Row] -> Table
table = return

zeroCostRow :: Row -> Row
zeroCostRow = row . value

zeroCostTable :: Table -> Table
zeroCostTable = table . map zeroCostRow . value

zeroCostHeaderTable :: Table -> Table
zeroCostHeaderTable = table . value

debugTable :: Table -> String
debugTable t =
  "pre cxty=" ++ show (cxty t) ++ "\n" ++
  (value t >>= debugRow) ++
  "Operations: " ++ show (complexityValue t)
  where
    debugRow r@(Complex es cs)
      | isFakeRow r = "post cxty=" ++ show es ++ "\n"
      | otherwise   = "row cxty=" ++ show es ++ " " ++ show cs ++ "\n"
