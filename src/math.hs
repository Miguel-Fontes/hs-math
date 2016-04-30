-- Combinações e Permutações ------------------------------------------------------------------------------------------
combinacoes :: (Ord a, Fractional a) => (a, a) -> a
combinacoes (n, r) = fatorial n / (fatorial r * fatorial(n - r))

permutacoes :: (Ord a, Fractional a) => (a, a) -> a
permutacoes (n, r) = (fatorial n) / (fatorial (n-r))

-- Fatorial -----------------------------------------------------------------------------------------------------------
-- Processo iterativo linear
fatorial :: (Num a, Ord a) => a -> a
fatorial x = factIter 1 1 x
    where factIter a b n
              | n > 0 = factIter (a * b) (b+1) (n-1)
              | otherwise = a

-- Processo recursivo linear
fatorial' :: (Num a, Ord a) => a -> a
fatorial' 1 = 1
fatorial' x = x * fatorial (x-1);

-- Números de Fibonacci -----------------------------------------------------------------------------------------------
fibonacci :: Int -> [Int]
fibonacci 1 = [1]
fibonacci 2 = [1,1]
fibonacci x = fibIter 1 1 [1,1]
    where fibIter a b fs
              | a + b < x = fibIter b (a+b) (a+b : fs)
              | otherwise = reverse (fs)


-- Triângulo de Pascal ------------------------------------------------------------------------------------------------
pascalTriangle :: Int -> [[Int]]
pascalTriangle 0 = [[]]
pascalTriangle r = step r 1 [[1]]
    where step r n xs
              | n == r = reverse xs
              | r >= n = step r (n+1) (([1] ++ (computeRow $ head xs) ++ [1]) : xs)
          computeRow xs
              | xs == [] = []
              | length xs > 1 = sum (take 2 xs) : computeRow (drop 1 xs)
              | otherwise = []

-- Pretty Printing para o Triangulo de Pascal. Cool.
printPascal :: Int -> IO()
printPascal r = do
    rows <- return $ map show (pascalTriangle r)
    let rowSize = (length . last) rows -- Uso o tamanho da última row como base para formatar
        parsedRows = foldr (\row acc -> addMargin ((rowSize - length row) `div` 2) row : acc) [] rows
    putStrLn $ (unlines) parsedRows

-- Identa uma margem à uma String qualquer com espaços
addMargin :: Int -> String -> String
addMargin x xs = (concat $ replicate x " ") ++ xs

-- Nümeros Primos -----------------------------------------------------------------------------------------------------
isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime x = primesIter x (x-1) 1
      where primesIter x d n
                | n > 2 = False
                | d == 0 = True
                | d > 0 = if x `mod` d == 0 then primesIter x (d-1) (n+1) else primesIter x (d-1) n

primosAte :: Integral a => a -> [a]
primosAte x = foldr step [] [2..x]
    where step x acc = if isPrime x then x : acc else acc