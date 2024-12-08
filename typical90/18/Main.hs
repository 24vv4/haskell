import Control.Monad

fn :: Double -> Double -> Double -> Double -> Double -> Double
fn l t x y et =
    let y' = - l * 0.5 * sin (2 * pi * et / t)
        yy = abs(y' - y)
        xy = sqrt(yy * yy + x * x)
        z = l * 0.5 + l * 0.5 * sin (2 * pi / t * (et - t / 4))
    in atan (z / xy)

main = do
    t <- readLn :: IO Double
    [l, x, y] <- map read . words <$> getLine :: IO [Double]
    q <- readLn :: IO Int
    replicateM_ q $ do
        et <- readLn :: IO Double
        let ans = fn l t x y et
        print $ ans * 180 / pi
