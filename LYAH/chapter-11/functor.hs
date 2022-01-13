import Data.Char
import Data.List

-- ordinary
-- main = do line <- getLine
--           let line' = reverse line
--           putStrLn $ "You said " ++ line' ++ " backwards!"
--           putStrLn $ "Yes, you said " ++ line' ++ " backwards."


-- using fmap
main = do line <- fmap reverse getLine
          putStrLn $ "You said " ++ line ++ " backwards."
          putStrLn $ "Yes, you said " ++ line ++ " backwards."
          line2 <- fmap (intersperse '-' . reverse . map toUpper) getLine
          putStrLn line2

myAction :: IO String
myAction = (++) <$> getLine <*> getLine

sequenceA2 :: (Applicative f) => [f a] -> f [a]
sequenceA2 [] = pure []
sequenceA2 (x:xs) = (:) <$> x <*> sequenceA2 xs