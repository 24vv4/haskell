import Control.Arrow
import Debug.Trace
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Bits
import qualified Data.Map.Strict as M

modulus :: Int
modulus = 10^9 + 7

modpow :: Int -> Int -> Int
modpow _ 0 = 1
modpow x n =
    if (n .&. 1) == 1 then x * (modpow (x*x`mod`modulus) (shiftR n 1)) `mod` modulus
    else (modpow (x*x`mod`modulus) (shiftR n 1)) `mod` modulus

readint = fmap (second B.tail) . B.readInt
main :: IO ()
main = do
    n <- (\vec -> (vec VU.! 0)) . VU.unfoldrN 1 readint <$> B.getLine
    let m = primeFactor n 2 M.empty
    let num = M.foldl (+) 0 m
    print $ count num 1 0

primeFactor:: Int -> Int -> M.Map Int Int -> M.Map Int Int
primeFactor 1 _ m = m
primeFactor n 1000010 m = M.alter f n m
primeFactor n x m =
    if n `mod` x == 0 then primeFactor (n `div` x) x $ M.alter f x m
    else primeFactor n (x+1) m

f :: Maybe Int -> Maybe Int
f Nothing = Just 1
f (Just x) = Just (x+1)

count :: Int -> Int -> Int -> Int
count num x ans
    | num <= x = ans
    | otherwise = count num (x*2) (ans+1)
