import Control.Arrow
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString.Char8 as B

-- This answer may be correct, but atcoder doesn't support B.readInt64 and got CE result.
readint64 = fmap (second B.tail) . B.readInt64
main = do
    (a,b,c) <- (\vec -> (vec VU.! 0, vec VU.! 1, vec VU.! 2)) . VU.unfoldrN 3 readint64 <$> B.getLine
    let g = gcd(gcd a b) c
    print $ a `div` g + b `div` g + c `div` g - 3
