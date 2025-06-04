{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  DBReader
-- Copyright   :  (c) 2019-2021 Konstantin Pugachev
-- License     :  MIT
--
-- Maintainer  :  K.V.Pugachev@inp.nsk.su
-- Stability   :  stable
-- Portability :  portable
--
-- The DBReader module reads a special database created
-- for the NSU.CS Declarative Programming: Query Languages course.
--
-----------------------------------------------------------------------------

module DBReader (readDB, readDB', defaultDBName) where

import CountableTable
import SQLHSSugar (Named, named)

-- package: sqlite-simple
-- http://hackage.haskell.org/package/sqlite-simple-0.4.16.0/docs/Database-SQLite-Simple.html
import Database.SQLite.Simple

readDB :: String -> IO (Table, Table, Table, Table)
readDB fname = do
  conn <- open fname
  categories <- query_ conn "SELECT WARE, CLASS FROM CATEGORY ORDER BY WARE DESC" :: IO [(String, String)]
  manufacturers <- query_ conn "SELECT RECIPE_ID, COMPANY from MANUFACTURER ORDER BY RECIPE_ID DESC" :: IO [(Int, String)]
  materials <- query_ conn "SELECT RECIPE_ID, WARE, AMOUNT from MATERIAL ORDER BY RECIPE_ID DESC" :: IO [(Int, String, Int)]
  products <- query_ conn "SELECT RECIPE_ID, WARE, AMOUNT, PRICE from PRODUCT ORDER BY RECIPE_ID DESC" :: IO [(Int, String, Int, Double)]
  close conn
  let categories' = map (\(a,b) -> row [StringCell a, StringCell b]) categories
  let manufacturers' = map (\(a,b) -> row [IntCell a, StringCell b]) manufacturers
  let materials' = map (\(a,b,c) -> row [IntCell a, StringCell b, IntCell c]) materials
  let products' = map (\(a,b,c,d) -> row [IntCell a, StringCell b, IntCell c, DoubleCell d]) products
  return (table categories', table manufacturers', table materials', table products')

readDB' :: String -> IO (Named Table, Named Table, Named Table, Named Table)
readDB' fname = do
  (categories, manufacturers, materials, products) <- readDB fname
  let categories' = named "CATEGORY" ["WARE", "CLASS"] categories
  let manufacturers' = named "MANUFACTURER" ["RECIPE_ID", "COMPANY"] manufacturers
  let materials' = named "MATERIAL" ["RECIPE_ID", "WARE", "AMOUNT"] materials
  let products' = named "PRODUCT" ["RECIPE_ID", "WARE", "AMOUNT", "PRICE"] products
  return (categories', manufacturers', materials', products')

defaultDBName = "wares.sqlite3"
