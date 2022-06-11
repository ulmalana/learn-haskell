import Control.Applicative

type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving Show

data PersonInvalid = EmptyName | AgeTooLow deriving (Eq, Show)

ageOK :: Age -> Either [PersonInvalid] Age
ageOK age =
    case age >= 0 of
        True -> Right age
        False -> Left [AgeTooLow]

nameOK :: Name -> Either [PersonInvalid] Name
nameOK name =
    case name /= "" of
        True -> Right name
        False -> Left [EmptyName]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age =
    liftA2 Person (nameOK name) (ageOK age)
