count :: String -> Char -> Int
count str c = length $ filter (==c) str
    
main = do
    s <- getLine
    putStrLn $ show (count s '1')
