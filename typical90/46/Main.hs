import Control.Arrow
import Debug.Trace
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Algorithms.Intro qualified as VAI
import qualified Data.Map.Strict as M

readint = fmap (second B.tail) . B.readInt
main :: IO ()
main = do
    n <- (\vec -> (vec VU.! 0)) . VU.unfoldrN 1 readint <$> B.getLine
    an <- VU.unfoldrN n readint <$> B.getLine
    bn <- VU.unfoldrN n readint <$> B.getLine
    cn <- VU.unfoldrN n readint <$> B.getLine
    let la = VU.map ((,1 :: Int) . (`mod` 46)) an
    let lb = VU.map ((,1 :: Int) . (`mod` 46)) bn
    let lc = VU.map ((,1 :: Int) . (`mod` 46)) cn
    let a = VU.accumulate (+) (VU.fromList $ take 46 $ repeat 0) la
    let b = VU.accumulate (+) (VU.fromList $ take 46 $ repeat 0) lb
    let c = VU.accumulate (+) (VU.fromList $ take 46 $ repeat 0) lc
    let ans = [(a VU.! i) * (b VU.! j) * (c VU.! k) | i <- [0..45], j <- [0..45], k <- [0..45], (i + j + k) `mod` 46 == 0]
    print $ sum ans
