judge :: Int -> String
judge x = 
    if x `mod` 2 == 0
        then "Even"
        else "Odd"

main = do 
    [a, b] <- map read . words <$> getLine
    putStrLn $ judge (a*b)
