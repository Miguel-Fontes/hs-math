fatorial :: Int -> Int
fatorial 1 = 1
fatorial x = x * fatorial (x-1);

combinacoes :: (Ord a, Fractional a) => (a, a) -> a
combinacoes (n, r) = factorial n / (factorial r * factorial(n - r))

permutacoes :: (Ord a, Fractional a) => (a, a) -> a
permutacoes (n, r) = (factorial n) / (factorial (n-r))


factorial :: (Num a, Ord a) => a -> a
factorial x = factIter 1 1 x
    where factIter a b n
              | n > 0 = factIter (a * b) (b+1) (n-1)
              | otherwise = a

fibonacci :: Int -> [Int]
fibonacci 1 = [1]
fibonacci 2 = [1,1]
fibonacci x = fibIter 1 1 [1,1]
    where fibIter a b fs
              | a + b < x = fibIter b (a+b) (a+b : fs)
              | otherwise = reverse (fs)

pascalTriangle :: Int -> [[Int]]
pascalTriangle 0 = [[]]
pascalTriangle 1 = [[1]]
pascalTriangle 2 = [[1,1],[1]]
pascalTriangle r = step (r-2) [[1,1],[1]]
    where step 0 xs = reverse xs
          step r xs = step (r-1) ( ([1] ++ (pascalRow (head xs)) ++ [1]) : xs)

pascalRow [] = []
pascalRow xs
    | length xs > 1 = sum (take 2 xs) : pascalRow (drop 1 xs)
    | otherwise = []

-- Pretty Printing para Pascal Triangles. Cool.
printPascal :: Int -> IO()
printPascal r = do
    pascals <- return $ map show (pascalTriangle r)
    let rowSize = (length . last) pascals -- Uma hora vai quebrar a linha no console e estragar a brincadeira
        parsed = foldr (\x acc -> addMargin ((rowSize - length x) `div` 2) x : acc) [] pascals
    putStrLn $ (unlines) parsed

-- Identa uma margem à uma String qualquer com espaços
addMargin :: Int -> String -> String
addMargin x xs = (concat $ replicate x " ") ++ xs

