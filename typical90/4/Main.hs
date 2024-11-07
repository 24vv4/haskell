import Control.Monad
import Data.List.Split
import Data.Array.IArray
import Data.Array.Unboxed

sumlist :: [Int] -> [Int] -> [Int]
sumlist [] a = a
sumlist a b = zipWith (+) a b

fn :: [Int] -> String
fn v = unwords . map show $ v

main = do
    [h, w] <- map read . words <$> getLine :: IO [Int]
    an <- replicateM h (map read . words <$> getLine)
    let ar = listArray @UArray ((0, 0), (h-1, w-1)) $ concat an
    let hsum = [sum [ar ! (i, j) | j <- [0..w-1]] | i <- [0..h-1]]
    let wsum = [sum [ar ! (i, j) | i <- [0..h-1]] | j <- [0..w-1]]
    let hr = listArray @UArray (0, h-1) hsum
    let wr = listArray @UArray (0, w-1) wsum

    let ans = [hr ! i + wr ! j - ar ! (i, j) | i <- [0..h-1], j <- [0..w-1]]

    forM_ (chunksOf w ans) $ \row -> do
        putStrLn $ fn row
