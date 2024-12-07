import Control.Arrow
import Debug.Trace
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro qualified as VAI
import qualified Data.Map.Strict as M
import Data.Bits

modulus :: Int
modulus = 10^9 + 7

modpow :: Int -> Int -> Int
modpow _ 0 = 1
modpow x n =
    if (n .&. 1) == 1 then x * (modpow (x*x`mod`modulus) (shiftR n 1)) `mod` modulus
    else (modpow (x*x`mod`modulus) (shiftR n 1)) `mod` modulus

primeFactor:: Int -> Int -> M.Map Int Int -> M.Map Int Int
primeFactor 1 _ m = m
primeFactor n 1000010 m = M.alter f n m
primeFactor n x m =
    if n `mod` x == 0 then primeFactor (n `div` x) x $ M.alter f x m
    else primeFactor n (x+1) m

-- TODO: remove
f :: Maybe Int -> Maybe Int
f Nothing = Just 1
f (Just x) = Just (x+1)

readint = fmap (second B.tail) . B.readInteger
main :: IO ()
main = do
    n <- (\vec -> (vec V.! 0)) . V.unfoldrN 1 readint <$> B.getLine
    s <- B.getLine
    print $ comb n - count s

count :: B.ByteString -> Integer
count s = count' s 1 1
    where
        count' :: B.ByteString -> Int -> Integer -> Integer
        count' s now con =
            if now == B.length s then comb con
            else if B.index s (now-1) == B.index s now then count' s (now+1) (con+1)
            else comb con + (count' s (now+1) 1)

comb :: Integer -> Integer
comb con = con * (con-1) `div` 2
