import Control.Monad.Writer
import Data.List

keepSmall :: Int -> Writer [String] Bool
keepSmall x 
 | x < 4 = do 
    tell ["Keeping " ++ show x]
    return True
 | otherwise = do 
    tell [show x ++ " is too large, throwing it away"]
    return False

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
 | x > 9 = Nothing
 | otherwise = Just (acc + x)

solveRPN :: String -> Maybe Double
-- solveRPN = head . foldM foldingFunction [] . words
solveRPN st = do 
    [result] <- foldM foldingFunction [] (words st)
    return result

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((x*y):ys)
foldingFunction (x:y:ys) "+" = return ((x+y):ys)
foldingFunction (x:y:ys) "-" = return ((y-x):ys)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a 
readMaybe st = case reads st of [(x,"")] -> Just x 
                                _ -> Nothing