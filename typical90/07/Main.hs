import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Algorithms.Intro qualified as VAI
import Control.Arrow

readint = fmap (second B.tail) . B.readInt

lowerBound :: VU.Vector Int -> Int -> Int
lowerBound v target = lb' v target (-1) (VU.length v)
    where
        -- lb' :: VU.Vector Int -> Int -> Int -> Int -> Int
        lb' v target ng ok = 
            if (ok - ng) <= 1 then ok
            else
                let mid = (ok + ng) `div` 2
                    num = v VU.! mid
                in if target <= num then lb' v target ng mid
                    else lb' v target mid ok


main = do
    n <- (\vec -> vec VU.! 0) . VU.unfoldrN 1 readint <$> B.getLine
    an <- VU.unfoldrN n readint <$> B.getLine
    q <- (\vec -> vec VU.! 0) . VU.unfoldrN 1 readint <$> B.getLine
    bq <- VU.replicateM q $ (\vec -> vec VU.! 0) . VU.unfoldrN 1 readint <$> B.getLine

    let san = VU.modify VAI.sort an
    let y = [let z = lowerBound san i in (max 0 (z-1), min (n-1) z) | i <- VU.toList bq]
    let z = VU.map (lowerBound san) bq
    let zm = VU.map (\i -> (max 0 (i-1), min (n-1) i)) z
    let ans = VU.zipWith (\(i, j) t -> min (abs(t - san VU.! i)) (abs(t - san VU.! j))) zm bq
    mapM_ print $ VU.toList ans
