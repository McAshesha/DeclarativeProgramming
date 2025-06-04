SELECT DISTINCT CLASS,
                (
                    SELECT WARE
                    FROM CATEGORY
                    WHERE CLASS = CAT.CLASS
                    ORDER BY WARE ASC
                    LIMIT 1
    ) AS WARE
FROM CATEGORY CAT;
