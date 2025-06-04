-----------------------------------------------------------------------------
-- |
-- Module      :  SQLHSSugar
-- Copyright   :  (c) 2021 Konstantin Pugachev
-- License     :  MIT
--
-- Maintainer  :  K.V.Pugachev@inp.nsk.su
-- Stability   :  experimental
-- Portability :  portable
--
-- The SQLHSSugar module provides wrappers for SQLHS module in order
-- to express operations using SQL-like Haskell syntax
-- accessing columns by string IDs
--
-----------------------------------------------------------------------------

module SQLHSSugar (
  Table,
  Named(..), named,
  str, int, double,
  eq,
  njoin, hjoin, mjoin, cjoin, wher, distinct, orderby, select,
  col, cols,
  limit, selectStar,
  indexby, flatten,
  on, jeq, asc, desc,
  (//), (&), columnId,
  printResult, enumerate
) where

import SQLHS hiding (col, cols, limit, selectStar, enumerate, printResult)
import qualified SQLHS
import CountableTable (value)

import qualified Data.Map as M
import Control.Monad (forM_, when)
import Data.List (intersect, intersperse, groupBy, sortBy)
import Data.Functor
import Data.Maybe
import Data.Function ((&))
import qualified Data.Function as F (on)

data Named a = Named { tcols :: [(String, String)], tcolids :: M.Map String Int, tbody :: a }

instance Functor Named where
  fmap f n = n { tbody = f (tbody n) }

named :: String -> [String] -> a -> Named a
named _ [] _              = error "Cannot name a zero width table"
named tname colnames body = Named { tcols = cols, tcolids = ids, tbody = body } where
  tname' = if tname /= "" then tname ++ "." else ""
  cols = map (\n -> (tname', n)) colnames
  ids = M.fromList $ enumerated colnames ++ enumerated (qualified' colnames)
  enumerated xs = zip xs [1..]
  qualified' = map (tname'++)

rename :: String -> Named a -> Named a
rename newname t
  | null cols                   = error "Cannot rename a zero width table"
  | any (/= head tnames) tnames = error $ "Cannot rename joined table (" ++ showColNames (tcols t) ++ ")"
  | otherwise                   = named newname cols (tbody t)
  where
    tnames = map fst . tcols $ t
    cols = map snd . tcols $ t

concatNames :: [(String, String)] -> [(String, String)] -> a -> Named a
concatNames lcols rcols t
  | length ambigous /= 0 = error $ "Cannot join. There are columns with ambigous names: " ++ showColNames ambigous
  | otherwise = Named { tcols = cols, tcolids = ids, tbody = t }
  where
    ambigous = lcols `intersect` rcols
    cols = lcols ++ rcols
    ids = M.fromList $ (uniqueItems . enumerated . unqualified $ cols) ++ (enumerated . qualified' $ cols)
    enumerated xs = zip xs [1..]
    unqualified = map snd
    qualified' = map (\(tn, cn) -> tn ++ cn)
    -- items that encounter only once
    uniqueItems :: (Ord a) => [(a,b)] -> [(a,b)]
    uniqueItems = map head . filter ((==1) . length) . groupBy ((==) `F.on` fst) . sortBy (compare `F.on` fst)

updateNames :: [(String, String)] -> a -> Named a
updateNames = concatNames []

liftNamed :: (a -> b -> c) -> (Named a -> Named b -> Named c)
liftNamed ordinaryJoin x y = concatNames (tcols x) (tcols y) $ ordinaryJoin (tbody x) (tbody y)

showColNames :: [(String, String)] -> String
showColNames = foldr1 (++) . intersperse ", " . map (\(tn, cn) -> tn ++ cn)

data PartialJoin c = PartialJoin (c -> Named Table)
type Projector = Named () -> (Row -> Key)
type Selector = Named () -> (Row -> Bool)

(<|) :: (Named () -> b) -> Named a -> b
f <| n = f $ n { tbody = () }

str :: String -> Projector
str s _ _ = row [StringCell s]

int :: Int -> Projector
int i _ _ = row [IntCell i]

double :: Double -> Projector
double d _ _ = row [DoubleCell d]

eq :: Projector -> Projector -> Selector
fa `eq` fb = \refTable -> \row -> (fa <| refTable) row == (fb <| refTable) row
infixl 4 `eq`

njoin :: Named Table -> Named Table -> PartialJoin (Projector, Projector)
x `njoin` y = PartialJoin $ \(f1, f2) -> liftNamed (naiveJoin (f1 <| x) (f2 <| y)) x y
infixl 2 `njoin`

hjoin :: Named Table -> Named HashedTable -> PartialJoin Projector
x `hjoin` y = PartialJoin $ \f1 -> liftNamed (hashJoin (f1 <| x)) x y
infixl 2 `hjoin`

mjoin :: Named Table -> Named Table -> PartialJoin (Projector, Projector)
x `mjoin` y = PartialJoin $ \(f1, f2) -> liftNamed (mergeJoin (f1 <| x) (f2 <| y)) x y
infixl 2 `mjoin`

cjoin :: Named Table -> Named Table -> Named Table
cjoin = liftNamed crossJoin
infixl 2 `cjoin`

indexby :: Named Table -> Projector -> Named HashedTable
indexby t p = tableToIndex (p <| t) <$> t
infixl 4 `indexby`

flatten :: Named HashedTable -> Named Table
flatten = fmap indexToTable

columnId :: Named a -> String -> Int
columnId table name = if isNothing cid
  then error $ "Cannot find column '" ++ name ++ "'. Please use one of: " ++ acceptableNames
  else fromJust cid
  where
    colids = tcolids table
    acceptableNames = foldr1 (++) . intersperse ", " . map fst . M.toList $ colids
    cid = M.lookup name colids

col :: String -> Named a -> (Row -> Row)
col name refTable = SQLHS.col $ columnId refTable name

cols :: [String] -> Named a -> (Row -> Row)
cols names refTable = SQLHS.cols $ map (columnId refTable) names

limit :: Int -> Int -> Named Table -> Named Table
limit x y = fmap (SQLHS.limit x y)

selectStar :: Named Table -> Named Table
selectStar = fmap SQLHS.selectStar

jeq :: String -> String -> (Projector, Projector)
x `jeq` y = (col x, col y)
infixl 3 `jeq`

on :: PartialJoin c -> c -> Named Table
(PartialJoin x) `on` y = x y
infixl 2 `on`

wher :: Named Table -> Selector -> Named Table
t `wher` s = naiveFilter (s <| t) <$> t
infixl 2 `wher`

asc :: [String]
asc = ["ASC"]

desc :: [String]
desc = ["DESC"]

(//) :: Named a -> String -> Named a
(//) = flip rename
infixl 5 //

orderby :: Named Table -> [[String]] -> Named Table
orderby t cs = orderBy (row . proj) <$> t where
  rawproj = value . cols (map head cs) t
  proj r = map maybeInvert . zip (map (head.tail) cs) $ rawproj r
  maybeInvert (i, c)
    | i=="DESC" = DescCell c
    | i=="ASC" = c
    | otherwise = error "use asc/desc"
infixl 2 `orderby`

select :: Named Table -> [String] -> Named Table
select t cs = updateNames names' t' where
  ids = map (\x -> x-1) . map (columnId t) $ cs
  names' = map (tcols t !!) ids
  t' = project (cols cs t) (tbody t)
infixl 2 `select`

distinct :: Named Table -> Named Table
distinct = fmap naiveDistinct

printResult :: Named Table -> IO ()
printResult t = do
  let body = tbody t
  let ops = complexityValue body
  let rows = filter (not . isFakeRow) (value body)
  let nrows = length rows
  
  putStrLn "Result:"
  
  putStr "["
  forM_ (zip [1..] (tcols t)) $ \(i,(tn,cn)) -> do
    when (i>1) $ putStr ","
    putStr $ tn ++ cn
  putStrLn "]"
  
  forM_ (take 10 rows) print
  putStrLn $ "Rows: " ++ show nrows ++ ". Operations: " ++ show ops

enumerate :: Named Table -> Named Table
enumerate t = concatNames [("","id")] (tcols t) $ SQLHS.enumerate (tbody t)
