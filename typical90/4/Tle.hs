import Control.Monad
import Data.List.Split

sumlist :: [Int] -> [Int] -> [Int]
sumlist [] a = a
sumlist a b = zipWith (+) a b

fn :: [Int] -> String
fn v = unwords . map show $ v

main = do
    [h, w] <- map read . words <$> getLine
    an <- replicateM h (map read . words <$> getLine) :: IO [[Int]]
    let hsum = map sum an
    let wsum = foldl sumlist [] an
    let ans = [hsum !! i + wsum !! j - (an !! i) !! j | i <- [0..h-1], j <- [0..w-1]]
    let y = map fn $ splitEvery w ans
    mapM_ putStrLn y
