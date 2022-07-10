import Control.Applicative (liftA2)

-- these newtypes are for making the difference explicit
newtype HumanName =
    HumanName String deriving (Eq, Show)

newtype DogName =
    DogName String deriving (Eq, Show)

newtype Address = 
    Address String deriving (Eq, Show)

-- record types based on newtypes above
data Person = 
    Person { humanName :: HumanName,
             dogName :: DogName,
             address :: Address
           } deriving (Eq, Show)

data Dog = 
    Dog { dogsName :: DogName,
          dogsAddress :: Address
        } deriving (Eq, Show)

-- new person
pers :: Person
pers = 
    Person (HumanName "Rick")
           (DogName "Doggie")
           (Address "Earth")

-- get Dog from Person
--
-- without Reader
getDog :: Person -> Dog
getDog p = 
    Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR = 
    Dog <$> dogName <*> address

-- with Reader in lift
getDogR' :: Person -> Dog
getDogR' =
    liftA2 Dog dogName address
