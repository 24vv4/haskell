import Control.Arrow
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Algorithms.Intro qualified as VAI

main :: IO ()
main = do
    n <- (\vec -> (vec VU.! 0)) . VU.unfoldrN 1 readint <$> B.getLine
    (a, b, c) <- (\vec -> (vec VU.! 0, vec VU.! 1, vec VU.! 2)) . VU.unfoldrN 3 readint <$> B.getLine
    let ans = [let k = (n - i*a - j*b) `div` c in i + j + k  | i <- [0..10000], j <- [0..10000], let x = (n - i*a - j*b) in x >= 0 && x `mod` c == 0]
    print $ minimum ans

readint = fmap (second B.tail) . B.readInt

lowerBound :: VU.Vector Int -> Int -> Int
lowerBound v target = lb' v target (-1) (VU.length v)
    where
        lb' :: VU.Vector Int -> Int -> Int -> Int -> Int
        lb' v target ng ok = 
            if (ok - ng) <= 1 then ok
            else
                let mid = (ok + ng) `div` 2
                    num = v VU.! mid
                in if target <= num then lb' v target ng mid
                    else lb' v target mid ok
