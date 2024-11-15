import Control.Arrow
import Data.List
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString.Char8 as B
main :: IO ()

readint = fmap (second B.tail) . B.readInt
main = do
    (n, p, q) <- (\vec -> (vec VU.! 0, vec VU.! 1, vec VU.! 2)) . VU.unfoldrN 3 readint <$> B.getLine
    an <- VU.unfoldrN n readint <$> B.getLine
    let mulmod x y = x * y `mod` p
    let ans = [ 1
            | a <- [0..n-1],
              b <- [a+1..n-1],
              c <- [b+1..n-1],
              d <- [c+1..n-1],
              e <- [d+1..n-1],
              let x = foldl mulmod 1 $ map (an VU.!) [a, b, c, d, e] in x == q
          ]
    print $ length ans
    return ()
