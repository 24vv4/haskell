import qualified Data.Set as DS
import qualified Data.ByteString.Char8 as B

solve :: [B.ByteString] -> [Int]
solve = solve' 1 DS.empty
    where 
        solve' :: Int -> DS.Set B.ByteString -> [B.ByteString] -> [Int]
        solve' _ _ [] = []
        solve' i set (w : ws)
            | DS.member w set = solve' (i+1) set ws
            | otherwise = i : solve' (i+1) (DS.insert w set) ws

main = do
    n <- readLn :: IO Int
    sn <- B.words <$> B.getContents
    let ans = solve sn
    mapM_ print ans
