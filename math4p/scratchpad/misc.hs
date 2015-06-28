count :: (Eq a) => a -> [a] -> Int
count p xs | p `elem` xs = 1
           | otherwise   = 0

countN :: (Eq a) => a -> [[a]] -> Int
countN p = sum . map (count p)

countNN :: (Eq a) => [a] -> [[a]] -> [Int]
countNN xs as = map (\x -> countN x as) xs

countCombine :: [Int] -> [Int] -> [Int]
countCombine ps xs = filter (/= 0) $ countNN ps $ combineWorst xs xs

combine :: Int -> [Int] -> [Int]
combine p = map (+p) 

combineN :: [Int] -> [Int] -> [[Int]]
combineN xs = map (\x -> combine x xs)

combineInc :: Int -> [Int] -> [Int]
combineInc p = map (+1) . combine p

combineIncN :: [Int] -> [Int] -> [[Int]]
combineIncN xs = map (\x -> combineInc x xs)

combineWorst :: [Int] -> [Int] -> [[Int]]
combineWorst xs ys = combineN xs ys ++ combineIncN xs ys
