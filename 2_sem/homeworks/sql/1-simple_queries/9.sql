SELECT ROUND(AVG(PRICE), 1) AS AvgPrice, ROUND(AVG(PRICE * PRICE) - AVG(PRICE) * AVG(PRICE), 1) AS VarPrice
FROM PRODUCT
WHERE WARE = 'Meat';
