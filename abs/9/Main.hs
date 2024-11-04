import Data.List

judge :: String -> Bool
judge [] = True
judge s = do
    if length s < 5 then False
    else if take 5 s == "maerd" then judge $ drop 5 s
    else if take 5 s == "esare" then judge $ drop 5 s
    else if length s >= 6 && take 6 s == "resare" then judge $ drop 6 s
    else if length s >= 7 && take 7 s == "remaerd" then judge $ drop 7 s
    else False
    
main = do
    s <- getLine
    if judge (reverse s) == True then putStrLn "YES"
    else putStrLn "NO"
