sum_digit :: Int -> Int
sum_digit 0 = 0
sum_digit x = x `mod` 10 + sum_digit (x `div` 10)

main = do
    [n, a, b] <- map read . words <$> getLine
    print $ sum [x | x <- [1..n], a <= sum_digit x, sum_digit x <= b]
