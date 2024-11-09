fn :: Int -> Int -> Int
fn a b = abs(a-b)
main = do
    [n, k] <- map read . words <$> getLine :: IO [Int]
    an <- map read . words <$> getLine :: IO [Int]
    bn <- map read . words <$> getLine :: IO [Int]
    let s = sum $ zipWith fn an bn
    putStrLn $ if s <= k && (k-s) `mod` 2 == 0 then "Yes" else "No"
