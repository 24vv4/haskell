import Control.Arrow
import Debug.Trace
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.List
import qualified Data.Set as DS

readint = fmap (second B.tail) . B.readInt
main :: IO ()

main = do
    n <- (\vec -> (vec VU.! 0)) . VU.unfoldrN 1 readint <$> B.getLine
    an <- V.replicateM n $ VU.unfoldrN n readint <$> B.getLine
    m <- (\vec -> (vec VU.! 0)) . VU.unfoldrN 1 readint <$> B.getLine
    xy <- VU.replicateM m $ (\vec -> ((vec VU.! 0) - 1 , (vec VU.! 1) - 1)) . VU.unfoldrN 2 readint <$> B.getLine
    
    let s = DS.fromList $ VU.toList xy
    let ans = [if not(valid s cand) then -1 else fn an cand | cand <- permutations [0..n-1]]
    let tans = filter (/= -1) ans
    print $ if length tans == 0 then -1 else minimum tans

fn :: V.Vector (VU.Vector Int) -> [Int] -> Int
fn an cand = 
    let h = [0..(length cand) - 1]
        t = zip h cand
        score = [((an V.! j) VU.! i) | (i, j) <- t]
    in sum score

valid :: DS.Set (Int, Int) -> [Int] -> Bool
valid s cand = 
    let candtuple = zip cand (tail cand)
        rcand = reverse cand
        rcandtuple = zip rcand (tail rcand)
        exist = [ct | ct <- candtuple, DS.member ct s]
        rexist = [ct | ct <- rcandtuple, DS.member ct s]
    in length exist == 0 && length rexist == 0
