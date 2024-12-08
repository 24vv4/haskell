import Control.Arrow
import Debug.Trace
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Algorithms.Intro qualified as VAI

readint = fmap (second B.tail) . B.readInt
main :: IO ()
main = do
    (n, k) <- (\vec -> (vec VU.! 0, vec VU.! 1)) . VU.unfoldrN 2 readint <$> B.getLine
    ab <- VU.replicateM n $ (\vec -> (vec VU.! 0, vec VU.! 1)) . VU.unfoldrN 2 readint <$> B.getLine
    let sorted = VU.modify VAI.sort $ VU.concatMap vfn ab
    print $ VU.sum $ VU.take k $ VU.reverse sorted

vfn :: (Int, Int) -> VU.Vector Int
vfn (a, b) = VU.singleton (a-b) VU.++ VU.singleton b
