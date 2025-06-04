-----------------------------------------------------------------------------
-- |
-- Module      :  SQLHSExample
-- Copyright   :  (c) 2019-2021 Konstantin Pugachev
--                (c) 2019 Denis Miginsky
-- License     :  MIT
--
-- Maintainer  :  K.V.Pugachev@inp.nsk.su
-- Stability   :  experimental
-- Portability :  portable
--
-- The SQLHSExample module provides examples of using SQLHS/SQLHSSugar module.
--
-----------------------------------------------------------------------------

import SQLHSSugar
import DBReader

-- CATEGORY:     WARE,    CLASS
-- MANUFACTURER: RECIPE_ID, COMPANY
-- MATERIAL:     RECIPE_ID, WARE,   AMOUNT
-- PRODUCT:      RECIPE_ID, WARE,   AMOUNT, PRICE

main = readDB' defaultDBName >>= executeSomeQueries

executeSomeQueries :: (Named Table, Named Table, Named Table, Named Table) -> IO ()
executeSomeQueries (categories, manufacturers, materials, products) = do
  -- test "lecPlan1'" lecPlan1'
  -- test "lecPlan2" lecPlan2
  -- test "lecPlan3" lecPlan3
  -- test "lecPlan4" lecPlan4
  -- test "lecPlan4'" lecPlan4'
  test "Plan 5.1 (2.3) efficient" lecPlan5_1
  test "Plan 5.2 triplets"       lecPlan5_2
  test "Plan 5.3 (2.8) efficient" lecPlan5_3

  
  where
    test msg p = do
      putStrLn $ "===== execute " ++ msg ++ " ====="
      -- putStrLn . debugTable $ p & enumerate
      printResult $ p & enumerate
    
    lecPlan1' = 
      -- MANUFACTURER NL_JOIN PRODUCT ON m.RECIPE_ID=p.RECIPE_ID
      manufacturers // "m" `njoin` products // "p" `on` "m.RECIPE_ID" `jeq` "p.RECIPE_ID"
      -- -> NL_JOIN CATEGORY ON c.WARE=p.WARE
      `njoin` categories // "c" `on` "p.WARE" `jeq` "c.WARE"
      -- -> FILTER c.CLASS='Raw food'
      `wher` col "CLASS" `eq` str "Raw food"
      -- -> SORT_BY p.WARE
      `orderby` ["p.WARE":asc]
      -- -> MAP (p.WARE, m.COMPANY)
      `select` ["p.WARE", "m.COMPANY"]
      -- -> DISTINCT
      & distinct 
      -- -> TAKE 10
      & limit 0 10
  
    lecPlan2 = 
      -- CATEGORY FILTER c.CLASS='Raw food'
      categories // "c" `wher` col "c.CLASS" `eq` str "Raw food"
      -- -> NL_JOIN PRODUCT ON c.WARE=p.WARE
      `njoin` products // "p" `on` "c.WARE" `jeq` "p.WARE"
      -- -> NL_JOIN MANUFACTURER ON m.RECIPE_ID=p.RECIPE_ID
      `njoin` manufacturers // "m" `on` "p.RECIPE_ID" `jeq` "m.RECIPE_ID"
      -- -> SORT_BY p.WARE
      `orderby` ["p.WARE":asc]
      -- -> MAP (p.WARE, m.COMPANY)
      `select` ["p.WARE", "m.COMPANY"]
      -- ->DISTINCT
      & distinct 
      -- -> TAKE 10
      & limit 0 10
  
    lecPlan3 = 
      -- CATEGORY FILTER c.CLASS='Raw food'
      categories // "c" `wher` col "c.CLASS" `eq` str "Raw food"  
      -- -> HASH_JOIN PRODUCT INDEX BY WARE ON c.WARE=p.WARE
      `hjoin` (products // "p" `indexby` col "WARE") `on` col "c.WARE"
      -- -> HASH_JOIN MANUFACTURER INDEX BY RECIPE_ID ON m.RECIPE_ID=p.RECIPE_ID
      `hjoin` (manufacturers // "m" `indexby` col "RECIPE_ID") `on` col "p.RECIPE_ID"
      -- -> SORT_BY p.WARE
      `orderby` ["p.WARE":asc]
      -- -> MAP (p.WARE, m.COMPANY)
      `select` ["p.WARE", "m.COMPANY"]
      -- ->DISTINCT
      & distinct 
      -- -> TAKE 10
      & limit 0 10

    lecPlan4 = 
      -- CATEGORY FILTER c.CLASS='Raw food'
      (categories // "c" `indexby` col "WARE" & flatten) `wher` col "CLASS" `eq` str "Raw food"
      -- -> MERGE_JOIN PRODUCT INDEX BY WARE ON c.WARE=p.WARE
      `mjoin` (products // "p" `indexby` col "WARE" & flatten) `on` "c.WARE" `jeq` "p.WARE"
      -- -> HASH_JOIN MANUFACTURER INDEX BY RECIPE_ID ON m.RECIPE_ID=p.RECIPE_ID
      `hjoin` (manufacturers // "m" `indexby` col "RECIPE_ID") `on` col "p.RECIPE_ID"
      -- -> MAP (p.WARE, m.COMPANY)
      `select` ["p.WARE", "m.COMPANY"]
      -- ->DISTINCT
      & distinct 
      -- -> TAKE 10
      & limit 0 10
  
    lecPlan4' = 
      -- CATEGORY FILTER c.CLASS='Raw food'
      (categories // "c" `indexby` col "WARE" & flatten) `wher` col "CLASS" `eq` str "Raw food"
      -- -> HASH_JOIN PRODUCT INDEX BY WARE ON c.WARE=p.WARE
      `hjoin` (products // "p" `indexby` col "WARE") `on` col "c.WARE"
      -- -> HASH_JOIN MANUFACTURER INDEX BY RECIPE_ID ON m.RECIPE_ID=p.RECIPE_ID
      `hjoin` (manufacturers // "m" `indexby` col "RECIPE_ID") `on` col "p.RECIPE_ID"
      -- -> MAP (p.WARE, m.COMPANY)
      `select` ["p.WARE", "m.COMPANY"]
      -- ->DISTINCT
      & distinct 
      -- -> TAKE 10
      & limit 0 10

    -- План для задачи 5.1 (SQL-2.3: все PRODUCT.WARE из материалов категории 'Mineral')
    -- 5.1. План для SQL-2.3
    lecPlan5_1 =
      categories // "c" `wher` col "c.CLASS" `eq` str "Mineral"
      `hjoin` (materials // "m" `indexby` col "WARE")      `on` col "c.WARE"
      `hjoin` (products  // "p" `indexby` col "RECIPE_ID") `on` col "m.RECIPE_ID"
      `select` ["p.WARE"]
      & distinct
      & (`orderby` ["p.WARE" : asc])


    -- 5.2. План для triplets (SQL-2.3 variant)
    -- 5.2. Оптимизированный план
    -- 5.2. План для triplets, без использования and_
    -- 5.2. Оптимизированный план: одна SQL-стейтмент с тремя JOIN’ами
    -- 5.2. Оптимизированный план с тремя JOIN’ами и без внешних подзапросов
    lecPlan5_2 =
      materials // "m"
        -- сразу берём только материалы из категории Mineral и индексируем по WARE
        `hjoin` ((categories // "c" `wher` col "c.CLASS" `eq` str "Mineral")
                  `indexby` col "WARE")
          `on` col "m.WARE"

        -- затем хеш-джойним продукты того же рецепта
        `hjoin` (products // "p" `indexby` col "RECIPE_ID")
          `on` col "m.RECIPE_ID"

        -- и сразу же из категории Stuff берём только нужные продукты
        `hjoin` ((categories // "c2" `wher` col "c2.CLASS" `eq` str "Stuff")
                  `indexby` col "WARE")
          `on` col "p.WARE"

        -- формируем результат
        `select` ["m.RECIPE_ID", "m.WARE", "p.WARE"]
        & distinct
        & (`orderby` ["m.RECIPE_ID" : asc])
        & limit 0 50







    -- 5.3. План для цепочек компаний (SQL-2.8)
    lecPlan5_3 =
      manufacturers // "m1"
      `hjoin` (manufacturers // "m2" `indexby` col "COMPANY") 
      `on` col "m1.COMPANY"
      `hjoin` (materials // "mat" `indexby` col "RECIPE_ID")
      `on` col "m1.RECIPE_ID"
      `hjoin` (products // "prod" `indexby` col "RECIPE_ID")
      `on` col "m2.RECIPE_ID"
      `wher` col "prod.WARE" `eq` col "mat.WARE"
      `orderby` ["m1.COMPANY":asc]
      `select` ["m1.COMPANY"]
      & distinct




