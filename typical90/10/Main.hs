import Control.Monad
import Data.List.Split
import Data.Array.IArray
import Data.Array.Unboxed

fn1 :: [Int] -> [Int] -> [Int]
fn1 a b
    | head b == 1 = [1, last a + last b]
    | otherwise = [1, last a]

fn2 :: [Int] -> [Int] -> [Int]
fn2 a b
    | head b == 2 = [2, last a + last b]
    | otherwise = [1, last a]

fn3 :: [Int] -> Int
fn3 a = last a

fn :: [[Int]] -> String
fn x = unwords . map show $ (head x)

main = do
    n <- readLn
    cp <- replicateM n (map read . words <$> getLine)
    let sumone = scanl (fn1) [0, 0] cp
    let sumtwo = scanl (fn2) [0, 0] cp
    let oar = listArray @UArray (0, n) $ map fn3 sumone
    let tar = listArray @UArray (0, n) $ map fn3 sumtwo

    q <- readLn
    lr <- replicateM q (map read . words <$> getLine) :: IO [[Int]]
    let qr = listArray @UArray ((0, 0), (q-1, 1)) $ concat lr

    let ans = [let [l, r] = [qr ! (i, 0), qr ! (i, 1)] in [oar ! r - oar ! (l - 1), tar ! r - tar ! (l - 1)] | i <- [0..q-1]]

    forM_ (chunksOf 1 ans) $ \row -> do
        putStrLn $ fn row
