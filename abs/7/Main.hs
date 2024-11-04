import Control.Monad
import Data.List

int = readLn
main = do
    n <- int
    d <- replicateM n int
    print $ length $ nub d
