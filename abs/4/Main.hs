int = readLn
main = do
    a <- int
    b <- int
    c <- int
    x <- int
    let l = [500 * na + 100 * nb + 50 * nc | na <- [0..a], nb <- [0..b], nc <- [0..c]]
    print $ length $ filter (==x) l
