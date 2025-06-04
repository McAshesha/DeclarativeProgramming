SELECT DISTINCT CLASS,
                (
                    SELECT CATEGORY.WARE
                    FROM CATEGORY, PRODUCT
                    WHERE CATEGORY.WARE = PRODUCT.WARE
                      AND CLASS = CAT.CLASS
                    ORDER BY PRICE DESC
                    LIMIT 1
    ) AS WARE,
       (
         SELECT PRICE
         FROM CATEGORY, PRODUCT
         WHERE CATEGORY.WARE = PRODUCT.WARE
           AND CLASS = CAT.CLASS
         ORDER BY PRICE DESC
         LIMIT 1
       ) AS PRICE
FROM CATEGORY CAT;
