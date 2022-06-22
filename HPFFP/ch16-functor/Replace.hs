-- Replace Experiment for lifting function

module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "Woohoo"]

-- making the argument more specific
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

-- > :t fmap replaceWithP
-- fmap replaceWithP :: Functor f => f a -> f Char
-- 
-- lifting replaceWithP once
liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

-- more specific definition of liftedReplace
liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

-- > :t (fmap . fmap) replaceWithP
-- (fmap . fmap) replaceWithP :: (Functor f1, Functor f) => f (f1 a) -> f (f1
-- Char)
--
-- lifting replaceWithP twice
twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

-- making more specific.
twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

-- lifting thrice
thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

-- making more specific
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

main :: IO ()
main = do
    putStr "replaceWithP' lms: "
    print (replaceWithP' lms)

    putStr "liftedReplace lms: "
    print (liftedReplace lms)

    putStr "liftedReplace' lms: "
    print (liftedReplace' lms)

    putStr "twiceLifted lms: "
    print (twiceLifted lms)

    putStr "twiceLifted' lms: "
    print (twiceLifted' lms)

    putStr "thriceLifted lms: "
    print (thriceLifted lms)

    putStr "thriceLifted' lms"
    print (thriceLifted' lms)
