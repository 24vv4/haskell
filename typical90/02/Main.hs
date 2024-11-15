import qualified Data.ByteString.Char8 as B
import Debug.Trace
import Control.Arrow
import qualified Data.Vector.Unboxed as VU

change :: Int -> Char
change 1 = '('
change (-1) = ')'

fn :: Int -> VU.Vector Int -> [String]
fn n v =
    if n == 0 then 
        let s = VU.sum v
            sc = VU.scanl (+) 0 v
            num = VU.length $ VU.filter (< 0) sc
            ans = VU.map change v
            x = VU.toList ans
            -- !_ = traceShow x ()
            -- !_ = traceShow num ()
            -- !_ = traceShow sc ()
        in
            if s /= 0 || num /= 0 then [""]
            else [x]

    else fn (n-1) (VU.snoc v 1) ++ fn (n-1) (VU.snoc v (-1))
    
readint = fmap (second B.tail) . B.readInt
main = do
    n <- (\vec -> vec VU.! 0) . VU.unfoldrN 1 readint <$> B.getLine
    let x = fn n VU.empty
    let y = filter (/= "") x
    mapM_ putStrLn y
