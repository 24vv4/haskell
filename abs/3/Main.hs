count :: Int -> Int
count x = inner 0 x
    where
        inner c x
            | x `mod` 2 == 1 = c
            | otherwise = inner (c+1) (x `div` 2)

main = do
    _ <- getLine
    an <- map read . words <$> getLine
    let cn = map count an
    print $ minimum cn
