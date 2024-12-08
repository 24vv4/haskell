import Control.Monad
import Control.Arrow
import Debug.Trace
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro qualified as VAI
import qualified Data.Vector.Unboxed.Mutable as MVU

readint = fmap (second B.tail) . B.readInt

main :: IO ()
main = do
    (n, q) <- (\vec -> (vec VU.! 0, vec VU.! 1)) . VU.unfoldrN 2 readint <$> B.getLine
    an <- (VU.unfoldrN n readint <$> B.getLine) >>= VU.thaw
    txy <- VU.replicateM q $ (\vec -> (vec VU.! 0, vec VU.! 1, vec VU.! 2)) . VU.unfoldrN 3 readint <$> B.getLine
    let rotate = 0
    (\f -> VU.foldM'_ f rotate txy) $ \rot (t, x, y) -> do
        let xindex = ((x - 1 - rot) + n) `mod` n
        let yindex = ((y - 1 - rot) + n) `mod` n
        if t == 1 then do
            MVU.swap an xindex yindex
            return rot
        else if t == 2 then return (rot + 1)
        else do
            print =<< MVU.read an xindex
            return rot
