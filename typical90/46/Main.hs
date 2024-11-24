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
    let ma = M.fromListWith (+) $ VU.toList la
    let mb = M.fromListWith (+) $ VU.toList lb
    let mc = M.fromListWith (+) $ VU.toList lc
    let a = M.toAscList ma
    let b = M.toAscList mb
    let c = M.toAscList mc
    let ans = [aj * bj * cj | (ai, aj) <- a, (bi, bj) <- b, (ci, cj) <- c, (ai + bi + ci) `mod` 46 == 0]
    print $ sum ans
