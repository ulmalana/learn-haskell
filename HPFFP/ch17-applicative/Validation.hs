data Validation err a = Failure err | Success a deriving (Eq, Show)

data Errors = DividedByZero | StackOverflow | MooglesChewedWires deriving (Eq, Show)

validToEither :: Validation e a -> Either e a
validToEither (Failure err) = Left err
validToEither (Success a) = Right a

eitherToValid :: Either e a -> Validation e a
eitherToValid (Left err) = Failure err
eitherToValid (Right a) = Success a

--eitherToValid . validToEither == id
--validToEither . eitherToValid == id 


