import Control.Arrow
import Debug.Trace
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as VU
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

readint = fmap (second B.tail) . B.readInt
main :: IO ()
main = do
    n <- (\vec -> (vec VU.! 0)) . VU.unfoldrN 1 readint <$> B.getLine
    an <- VU.unfoldrN n readint <$> B.getLine
    let bn = an VU.++ an
    let target = VU.sum an
    if target `mod` 10 /= 0 then putStrLn "No"
    else do
        let ans = twoPointer bn 0 0 0 (target `div` 10)
        putStrLn $ if ans then "Yes" else "No"

twoPointer :: VU.Vector Int -> Int -> Int -> Int -> Int -> Bool
twoPointer v l r sum target
    | sum == target = True
    | VU.length v == r && sum < target = False
    | sum < target = twoPointer v l (r+1) (sum + v VU.! r) target
    | otherwise = twoPointer v (l+1) r (sum - v VU.! l) target
