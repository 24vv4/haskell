import Control.Arrow
import Debug.Trace
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Algorithms.Intro qualified as VAI

readint = fmap (second B.tail) . B.readInt
main :: IO ()
main = do
    [a, b] <- map read . words <$> getLine :: IO [Integer]
    let l = lcm a b
    putStrLn $ if l > 10^18 then "Large" else show l
