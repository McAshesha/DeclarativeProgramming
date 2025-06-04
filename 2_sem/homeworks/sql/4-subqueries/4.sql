SELECT DISTINCT COMPANY
FROM MANUFACTURER
WHERE COMPANY NOT IN
      (
          SELECT DISTINCT COMPANY
          FROM MANUFACTURER, PRODUCT,
               (
                   SELECT WARE,
                          AVG(PRICE) AS AVGPRICE
                   FROM PRODUCT
                   GROUP BY WARE
               ) SUB
          WHERE MANUFACTURER.RECIPE_ID = PRODUCT.RECIPE_ID
            AND PRODUCT.WARE = SUB.WARE
            AND PRICE <= SUB.AVGPRICE * 1.2
          GROUP BY COMPANY
      );
