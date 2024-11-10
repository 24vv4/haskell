import Control.Arrow
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString.Char8 as B

readint = fmap (second B.tail) . B.readInt

solve :: VU.Vector (Int, Int) -> [Int]
solve = solve' VU.empty
    where
        solve' :: VU.Vector Int -> VU.Vector (Int, Int) -> [Int]
        solve' deck nx =
            if VU.null nx then []
            else 
                let (t, x) = nx VU.! 0
                in case t of 
                    1 -> solve' (VU.cons x deck) (VU.tail nx)
                    2 -> solve' (VU.snoc deck x) (VU.tail nx)
                    3 -> deck VU.! (x-1) : solve' deck (VU.tail nx)
        
main = do
    n <- (\vec -> (vec VU.! 0)) . VU.unfoldrN 1 readint <$> B.getLine
    tx <- VU.replicateM n $ (\vec -> (vec VU.! 0, vec VU.! 1)) . VU.unfoldrN 2 readint <$> B.getLine
    mapM_ print $ solve tx
