comprehensive xs ys = [(x,y) | x <- xs, y <- ys]

monadic xs ys = do { x <- xs; y <- ys; return (x,y) }

blockDo xs ys = do 
    x <- xs
    y <- ys
    return (x,y)

blockPlain xs ys = 
    xs >>=
    \x -> ys >>=
    \y -> return (x, y)

blockPlain2 xs ys =
    concat (map (\x ->
                  concat (map (\y ->
                                return (x,y))
                          ys))
            xs)