main = do
    line1 <- getLine
    let a = read line1 :: Int
    
    line2 <- getLine
    let [(b, line2')] = reads line2 :: [(Int, String)]
    let c = read line2' :: Int

    s <- getLine
    putStrLn $ show (a + b + c) ++ " " ++ s 
