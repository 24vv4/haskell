main = do
    [a, b, c] <- map read . words <$> getLine :: IO [Integer]
    let g = gcd(gcd a b) c
    print $ a `div` g + b `div` g + c `div` g - 3
