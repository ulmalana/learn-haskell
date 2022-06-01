-- higher order functions

data Employee = Coder
                | Manager
                | Veep
                | CEO
                deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
    putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' = case compare e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e' -- manipulate fn with HOF

-- refactor employeeRank so that it can take custome comparison function
employeeRank' :: (Employee -> Employee -> Ordering)
                -> Employee
                -> Employee
                -> IO ()
employeeRank' f e e' = case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither is the boss"
    LT -> (flip reportBoss) e e'

-- this custom comparison function can be used for refactor employeeRank'
-- above to prioritise the Coder in the result
coderRuleCEODrool :: Employee -> Employee -> Ordering
coderRuleCEODrool Coder Coder = EQ
coderRuleCEODrool Coder _ = GT
coderRuleCEODrool _ Coder = LT
coderRuleCEODrool e e' = compare e e'
