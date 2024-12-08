import Control.Arrow
import Debug.Trace
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as MVU
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
    (h, w) <- (\vec -> (vec VU.! 0, vec VU.! 1)) . VU.unfoldrN 2 readint <$> B.getLine
    an <- V.replicateM h $ VU.unfoldrN w readint <$> B.getLine >>= VU.thaw
    bn <- V.replicateM h $ VU.unfoldrN w readint <$> B.getLine
    ans <- modify an bn 0
    same <- check an bn 0
    if same then do
        putStrLn "Yes"
        print ans
    else putStrLn "No"
 
modify an bn pos = do
    let h = V.length bn
    let w = VU.length (bn V.! 0)
    let posh = pos `div` w
    let posw = pos `mod` w
    if posw == w-1 then return =<< modify an bn (pos+1)
    else do
        let b = (bn V.! posh) VU.! posw
        a <- MVU.read (an V.! posh) posw
        let diff = b - a
        MVU.modify (an V.! posh) (+diff) posw
        MVU.modify (an V.! (posh+1)) (+diff) posw
        MVU.modify (an V.! posh) (+diff) (posw+1)
        MVU.modify (an V.! (posh+1)) (+diff) (posw+1)
        if posh == h-2 && posw == w-2 then return $ abs(diff)
        else do
            r <- modify an bn (pos+1)
            return $ abs(diff) + r

check an bn pos = do
    let h = V.length bn
    let w = VU.length (bn V.! 0)
    let posh = pos `div` w
    let posw = pos `mod` w
    let b = (bn V.! posh) VU.! posw
    a <- MVU.read (an V.! posh) posw
    if posh == h-1 && posw == w-1 then return $ a == b
    else do
        r <- check an bn (pos+1)
        return $ (a==b) && r
