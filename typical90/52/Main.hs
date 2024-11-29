import Control.Arrow
import Debug.Trace
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro qualified as VAI

modulus :: Int
modulus = 10^9 + 7

readint = fmap (second B.tail) . B.readInt
main :: IO ()
main = do
    n <- (\vec -> (vec VU.! 0)) . VU.unfoldrN 1 readint <$> B.getLine
    an <- V.replicateM n $ VU.unfoldrN 6 readint <$> B.getLine
    let sn = V.map VU.sum an
    print $ V.foldl (\x y -> x * y `mod` modulus) 1 sn
