import Control.Arrow
import Debug.Trace
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Algorithms.Intro qualified as VAI

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
main :: IO ()
main = do
    --(n, m) <- (\vec -> (vec VU.! 0, vec VU.! 1)) . VU.unfoldrN 2 readint <$> B.getLine
    -- !_ = traceShow n ()
