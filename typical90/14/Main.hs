import Control.Arrow
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Algorithms.Intro qualified as VAI

main :: IO ()
main = do
    n <- (\vec -> vec VU.! 0) . VU.unfoldrN 1 readint <$> B.getLine
    an <- VU.unfoldrN n readint <$> B.getLine
    bn <- VU.unfoldrN n readint <$> B.getLine
    let san = VU.modify VAI.sort an
    let sbn = VU.modify VAI.sort bn
    let diff = VU.zipWith (-) san sbn
    let ans = VU.sum $ VU.map abs diff
    print ans

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
