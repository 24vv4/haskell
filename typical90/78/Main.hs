import Control.Arrow
import Debug.Trace
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString.Char8 as B

readint = fmap (second B.tail) . B.readInt

fn :: M.Map Int Int -> VU.Vector (Int, Int) -> M.Map Int Int
fn m ab =
    if VU.null ab then m
    else 
        let (a, b) = ab VU.! 0
            -- !_ = traceShowId m
        in fn (M.update (\x -> Just (x+1)) (max a b) m) (VU.tail ab)

main :: IO ()
main = do
    (n, m) <- (\vec -> (vec VU.! 0, vec VU.! 1)) . VU.unfoldrN 2 readint <$> B.getLine
    ab <- VU.replicateM m $ (\vec -> (vec VU.! 0, vec VU.! 1)) . VU.unfoldrN 2 readint <$> B.getLine
    let mp = fn (M.fromList [(i, 0)| i <- [1..n]]) ab
    print $ length $ M.filter (== 1) mp
