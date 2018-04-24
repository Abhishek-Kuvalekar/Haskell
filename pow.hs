-- Calculates x raised to y.
pow :: Int -> Int -> Int
pow x y 
    | y == 0 = 1
    | otherwise = x * pow x (y - 1)
