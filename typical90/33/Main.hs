solve h w = 
    if h == 1 || w == 1 then h * w
    else
        let h2 = (h+1) `div` 2
            w2 = (w+1) `div` 2
        in h2 * w2

main = do
    [h, w] <- map read . words <$> getLine :: IO [Int]
    print $ solve h w
