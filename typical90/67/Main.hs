import Debug.Trace
import Data.Char

times :: Int -> (a -> a) -> a -> a
times n fn x = 
    if n == 0 then x
    else times (n-1) fn (fn x)

eightToNine :: String -> String
eightToNine x = (fn9 . fn10) x

fn10 :: String -> Int
fn10 s = 
    let en = [8 ^ i | i <- [0..20]]
        sn = map charToInt s
        res = sum $ zipWith (*) sn en
    in res

fn9 :: Int -> String
fn9 0 = []
fn9 x = intToChar (x `mod` 9) : fn9 (x `div` 9)

charToInt :: Char -> Int
charToInt al = fromEnum al - 48

intToChar :: Int -> Char
intToChar i =
    if i == 8 then '5'
    else toEnum $ 48 + i
    
main :: IO ()
main = do
    [n, k] <- words <$> getLine
    let ans = reverse $ times (read k) eightToNine $ reverse n
    putStrLn $ if ans == [] then "0" else ans
