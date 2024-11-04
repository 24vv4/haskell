main = do
    [n, y] <- map read . words <$> getLine
    let ij = [(i, j, n-i-j) | i <- [0..n], j <- [0..n], i+j <= n, 10000 * i + 5000 * j + (n-i-j) * 1000 == y]
    if length ij == 0 then putStrLn "-1 -1 -1"
    else do
        let (a, b, c) = head ij
        putStrLn $ show(a) ++ " " ++ show(b) ++ " " ++ show(c)
