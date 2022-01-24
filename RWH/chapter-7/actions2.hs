st2msg :: String -> String
st2msg input = "Data: " ++ input

str2action :: String -> IO ()
str2action = putStrLn . st2msg

numbers :: [Int]
numbers = [1..10]

main = do str2action "Start of the program"
          mapM_ (str2action . show) numbers
          str2action "Done"