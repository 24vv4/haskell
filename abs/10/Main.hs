import Control.Monad
int = readLn

judge :: [[Int]] -> Bool
judge txy = inner 0 0 0 txy
    where 
        inner x y t txy
            | length txy == 0 = True
            | otherwise = do
                let p = head txy
                let nt = p !! 0
                let nx = p !! 1
                let ny = p !! 2
                let d = abs(nx - x) + abs(ny - y)
                let dt = nt - t
                if d > dt then False
                else if d `mod` 2 /= dt `mod` 2 then False
                else inner nx ny nt (drop 1 txy)

test :: [Int] -> [Int] -> Bool
test a b = do
    let [lt, lx, ly] = a
    let [rt, rx, ry] = b
    let d = abs(lx - rx) + abs(ly - ry)
    let dt = rt - lt
    if d > dt then False
    else if d `mod` 2 /= dt `mod` 2 then False
    else True

main = do
    n <- int
    txy <- replicateM n (map read . words <$> getLine) :: IO [[Int]]
    let l = [0, 0, 0] : txy
    let r = drop 1 l
    let ans = foldl (&&) True $ zipWith test l r
    putStrLn $ if ans then "Yes" else "No"

    -- TODO: Results of this logic is TLE.
    -- putStrLn $ if judge txy then "Yes" else "No"
