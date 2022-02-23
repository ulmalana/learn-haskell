import System.Environment
import Text.Printf

mean :: [Double] -> Double
mean xs = s / fromIntegral n 
  where 
    (n, s) = foldl k (0,0) xs
    k (n,s) x = (n+1, s+x)

main = do 
    [d] <- map read `fmap` getArgs
    printf "%f\n" (mean [1..d])