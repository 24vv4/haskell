judge :: (Int, Int, Int, Int) -> Int
judge (a, b, c, x) = 
    if 500 * a + 100 * b + 50 * c == x then 1 else 0

main = do
    a <- readLn :: IO Int
    b <- readLn :: IO Int
    c <- readLn :: IO Int
    x <- readLn :: IO Int
    let l = [(na, nb, nc, x) | na <- [0..a], nb <- [0..b], nc <- [0..c], x <- [x..x]]
    print $ sum (map judge l)
