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
    let mab = VU.map fn ab
    let (a, b) = VU.unzip mab
    let c = a VU.++ b
    let sorted = VU.modify VAI.sort c
    print $ VU.sum $ VU.drop (2*n-k) sorted

fn :: (Int, Int) -> (Int, Int)
fn (a, b) = (a-b, b)
