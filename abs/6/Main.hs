import Data.List

el :: [a] -> [a]
el [] = []
el [_] = []
el (_:x:xs) = x : el xs

main = do
    _ <- getLine
    a <- map read . words <$> getLine
    let ra = (reverse . sort) a
    let bob = sum $ el ra
    let alice = sum a - bob
    print $ alice - bob
