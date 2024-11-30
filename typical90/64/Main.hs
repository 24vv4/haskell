import Control.Monad
import Control.Arrow
import Debug.Trace
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import Data.Vector.Algorithms.Intro qualified as VAI

readint = fmap (second B.tail) . B.readInt
main :: IO ()
main = do
    (n, q) <- (\vec -> (vec VU.! 0, vec VU.! 1)) . VU.unfoldrN 2 readint <$> B.getLine
    an <- VU.unfoldrN n readint <$> B.getLine
    let cn = VU.zipWith (-) an (VU.tail an)
    let size = VU.length cn
    let tmp = VU.sum $ VU.map abs cn

    ans <- VU.thaw $ VU.singleton tmp
    vn <- VU.thaw cn

    replicateM_ q $ do
        (l, r, v) <- (\vec -> (vec VU.! 0, vec VU.! 1, vec VU.! 2)) . VU.unfoldrN 3 readint <$> B.getLine
        if r-1 < size then do
            y <- MVU.read vn (r-1)
            let ny = y + v
            MVU.write vn (r-1) ny
            tmp <- MVU.read ans 0
            MVU.write ans 0 (tmp + abs(ny) - abs(y))
        else return ()

        if l-2 >= 0 then do
            y <- MVU.read vn (l-2)
            let ny = y - v
            MVU.write vn (l-2) ny
            tmp <- MVU.read ans 0
            MVU.write ans 0 (tmp + abs(ny) - abs(y))
        else return ()
        print =<< MVU.read ans 0
