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
    (l, r) <- (\vec -> (vec V.! 0, vec V.! 1)) . V.unfoldrN 2 readint <$> B.getLine
    let ans = count l r
    print $ ans `mod` (10^9 + 7)

count :: Integer -> Integer -> Integer
count l r =
    let f x = until (> x) (*10) (1::Integer)
        nx = f l
        d = nd l
    in if nx > r then d * (sumd l r)
    else d * (sumd l (nx-1)) + count nx r

nd :: Integer -> Integer
nd = nd' 1
    where
        nd' :: Integer -> Integer -> Integer
        nd' x start =
            if x < start * 10 then 1
            else 1 + (nd' x (start * 10))

sumd :: Integer -> Integer -> Integer
sumd l r = (l + r) * (r - l + 1) `div` 2
