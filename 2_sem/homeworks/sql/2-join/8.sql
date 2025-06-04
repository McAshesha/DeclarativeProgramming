SELECT DISTINCT m1.COMPANY
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
ORDER BY m1.COMPANY ASC;
