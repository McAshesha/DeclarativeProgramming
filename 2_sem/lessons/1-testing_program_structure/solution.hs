import Test.Hspec

-- 1 Функция, считающая частоту каждого элемента
frequencies :: Eq a => [a] -> [(a, Int)]
frequencies = foldl f []
    where
        f [] a = [(a, 1)]
        f ((x, count):xs) a
            | a == x = (x, count + 1) : xs
            | otherwise = (x, count) : f xs a

-- Три теста для frequencies
main :: IO ()
main = hspec $ do
    describe "My frequencies " $ do
        it "returns the void list" $ do
            frequencies [] `shouldBe` ([] :: [(Int, Int)])
        it "counts frequencies in a list with duplicates" $ do
            frequencies [1, 2, 2, 3, 3, 3] `shouldBe` [(1, 1), (2, 2), (3, 3)]
        it "counts frequencies in a list with a single unique element" $ do
            frequencies ["hello"] `shouldBe` [("hello", 1)]
