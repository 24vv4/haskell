import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as VU


main :: IO ()
times :: Int -> (a -> a) -> a -> a
times n fn x = 
    if n == 0 then x
    else times (n-1) fn (fn x)

eightToNine :: B.ByteString -> B.ByteString
eightToNine x = (fn9 . fn10) x

fn10 :: B.ByteString -> Int
fn10 x =
    let en = [8 ^ i | i <- [0..20]]
        fold

fn9 :: Int -> B.ByteString
fn9 x = B.empty


    
main = do
    [n, k] <- B.words <$> B.getLine
    let (kk, _) = maybe (0, B.empty) id (B.readInt k)
    print $ times kk eightToNine $ B.reverse n
    let en = [8 ^ i | i <- [0..20]]
    print en
    

    
