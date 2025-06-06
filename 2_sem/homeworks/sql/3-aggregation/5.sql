SELECT
    MANUFACTURER.RECIPE_ID AS Recipe_ID,
    MANUFACTURER.COMPANY,
    GROUP_CONCAT(DISTINCT PRODUCT.WARE) AS Products,
    GROUP_CONCAT(DISTINCT MATERIAL.WARE) AS Materials
FROM MANUFACTURER
         JOIN PRODUCT ON MANUFACTURER.RECIPE_ID = PRODUCT.RECIPE_ID
         LEFT JOIN MATERIAL ON MANUFACTURER.RECIPE_ID = MATERIAL.RECIPE_ID
GROUP BY MANUFACTURER.RECIPE_ID
ORDER BY MANUFACTURER.COMPANY ASC;
