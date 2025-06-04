SELECT fst.COMPANY,
       '(' || COALESCE(GROUP_CONCAT(DISTINCT m_fst.WARE), '') || ')-[' || fst.RECIPE_ID || ']->(' ||
       p_fst.WARE || ')-[' || snd.RECIPE_ID || ']->(' || GROUP_CONCAT(DISTINCT p_snd.WARE) || ')' as CHAIN
FROM MANUFACTURER fst
         JOIN MANUFACTURER snd ON fst.COMPANY = snd.COMPANY
         LEFT JOIN MATERIAL m_fst ON fst.RECIPE_ID = m_fst.RECIPE_ID
         JOIN PRODUCT p_fst ON fst.RECIPE_ID = p_fst.RECIPE_ID
         JOIN MATERIAL m_snd ON snd.RECIPE_ID = m_snd.RECIPE_ID AND p_fst.WARE = m_snd.WARE
         JOIN PRODUCT p_snd ON snd.RECIPE_ID = p_snd.RECIPE_ID
GROUP BY fst.COMPANY, p_fst.WARE, p_fst.RECIPE_ID, p_snd.RECIPE_ID
ORDER BY fst.COMPANY ASC
