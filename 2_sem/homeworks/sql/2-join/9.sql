SELECT DISTINCT m1.COMPANY, p1.WARE AS WARE1, m.WARE AS WARE2, p2.WARE AS WARE3
FROM PRODUCT p1
         INNER JOIN MATERIAL m
                    ON p1.WARE = m.WARE        -- то, что произвели — используется как материал
         INNER JOIN PRODUCT p2
                    ON m.RECIPE_ID = p2.RECIPE_ID  -- p2 — рецепт, в который входит этот материал
         INNER JOIN MANUFACTURER m1
                    ON p1.RECIPE_ID = m1.RECIPE_ID
         INNER JOIN MANUFACTURER m2
                    ON p2.RECIPE_ID = m2.RECIPE_ID
WHERE m1.COMPANY = m2.COMPANY              -- одна и та же компания делает и p1, и p2
ORDER BY WARE2 ASC;

SELECT DISTINCT co1.COMPANY, m2.WARE, p2.WARE, p1.WARE

FROM MANUFACTURER co1
         JOIN MANUFACTURER co2
              ON co1.COMPANY = co2.COMPANY
         JOIN PRODUCT p1
              ON co1.RECIPE_ID = p1.RECIPE_ID
         JOIN MATERIAL m1
              ON co1.RECIPE_ID = m1.RECIPE_ID
         JOIN PRODUCT p2
              ON co2.RECIPE_ID = p2.RECIPE_ID AND m1.WARE = p2.WARE
         LEFT OUTER JOIN MATERIAL m2
                         ON co2.RECIPE_ID = m2.RECIPE_ID
ORDER BY co1.COMPANY ASC
