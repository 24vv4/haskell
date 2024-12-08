import Control.Arrow
import Debug.Trace
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Bits

modulus :: Int
modulus = 10^9 + 7

readint = fmap (second B.tail) . B.readInt
main :: IO ()
main = do
    (n, k) <- (\vec -> (vec VU.! 0, vec VU.! 1)) . VU.unfoldrN 2 readint <$> B.getLine
    let ans = modpow (k-2) (n-2)
    if n == 1 then print k
    else print $ fn k $ fn (k-1) ans

fn :: Int -> Int -> Int
fn x y = x * y `mod` modulus

modpow :: Int -> Int -> Int
modpow _ 0 = 1
modpow x n =
    if (n .&. 1) == 1 then x * (modpow (x*x`mod`modulus) (shiftR n 1)) `mod` modulus
    else (modpow (x*x`mod`modulus) (shiftR n 1)) `mod` modulus
