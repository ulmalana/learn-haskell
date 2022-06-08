-- construct and deconstruct values

data GuessWhat = Chickenbutt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a 
             | Second b
             deriving (Eq, Show)

data RecordProduct a b = 
    RecordProduct { pfirst :: a
                  , psecond :: b } deriving (Eq, Show)

-- ###### sum and product ######
newtype NumCow = NumCow Int deriving (Eq, Show)

newtype NumPig = NumPig Int deriving (Eq, Show)
-- Farmhouse and Farmhouse' are the same
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig

-- to support more than 2 values, we can nest them
newtype NumSheep = NumSheep Int deriving (Eq, Show)
-- BigFarmhouse and BigFarmhouse' are the same
data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

-- nesting values in sum type
type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)
-- Animal and Animal' are the same
data Animal = Cow CowInfo
            | Pig PigInfo
            | Sheep SheepInfo
            deriving (Eq, Show)
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)
