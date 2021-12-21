main = do
    -- a <- getLine
    -- b <- getLine
    -- c <- getLine
    -- print [a,b,c]

    --same as
    
    rs <- sequence [getLine, getLine, getLine]
    print rs