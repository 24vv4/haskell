import Control.Arrow
import Debug.Trace
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro qualified as VAI

modulus :: Int
modulus = 10^9 + 7

newtype IntMod = IntMod Int deriving (Eq, Show)

instance Num IntMod where
  IntMod x + IntMod y = IntMod ((x + y) `mod` modulus)
  IntMod x - IntMod y = IntMod ((x - y) `mod` modulus)
  IntMod x * IntMod y = IntMod ((x * y) `mod` modulus)
  fromInteger n = IntMod (fromInteger (n `mod` fromIntegral modulus))
  abs = abs
  signum = signum

readint = fmap (second B.tail) . B.readInt
main :: IO ()
main = do
    (n, ll) <- (\vec -> (vec VU.! 0, vec VU.! 1)) . VU.unfoldrN 2 readint <$> B.getLine
    let dp :: V.Vector IntMod
        dp = V.constructN (n+1) f
            where
                f :: V.Vector IntMod -> IntMod
                f an = 
                    if V.null an then 1
                    else
                        let len = V.length an
                        in if len - ll >= 0 then an V.! (len-ll) + an V.! (len-1)
                        else an V.! (len-1)

    let ans = dp
    let (IntMod p) = ans V.! n
    print p
