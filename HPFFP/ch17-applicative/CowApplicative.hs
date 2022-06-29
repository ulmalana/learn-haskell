import Control.Applicative

data Cow = Cow {
    name :: String,
    age :: Int,
    weight :: Int } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

-- validating in the input
-- complicated version
cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' = 
    case noEmpty name' of
        Nothing -> Nothing
        Just namey ->
            case noNegative age' of
                Nothing -> Nothing
                Just agey ->
                    case noNegative weight' of
                        Nothing -> Nothing
                        Just weighty -> 
                            Just (Cow namey agey weighty)

-- applicative version
cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name' age' weight' =
    Cow <$> noEmpty name'
        <*> noNegative age'
        <*> noNegative weight'

-- using liftA3 version
cowFromString'' :: String -> Int -> Int -> Maybe Cow
cowFromString'' name' age' weight' =
    liftA3 Cow (noEmpty name')
               (noNegative age')
               (noNegative weight')
