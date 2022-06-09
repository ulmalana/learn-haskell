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

-- ### Constructing values ###

-- Nullary
-- GuessWhat is equivalent to () unit type
trivialValue :: GuessWhat 
trivialValue = Chickenbutt

-- Unary type constructor
idInt :: Id Integer
idInt = MkId 9

-- since functions are values, we can put function to Id
idIdentity :: Id (a -> a)
idIdentity = MkId $ \x -> x

-- Product type
type Hungry = Bool

person :: Product Name Hungry
person = Product "Riz" True

-- Sum type
data Twitter = Twitter deriving (Eq, Show)
data AskFm = AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

-- this will fail
-- socialNetwork' :: Sum Twitter AskFm
-- socialNetwork' = First AskFm

-- Record
myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 12 0.5

myRecord' :: RecordProduct Integer Float
myRecord' = RecordProduct { pfirst = 24, psecond = 88.5 }

-- exercise
data OSs = Linux | BSD | Mac | Windows deriving (Eq, Show)
data PLs = Haskell | Agda | Idris | PureScript deriving (Eq, Show)

data Programmer = Programmer { os :: OSs, lang :: PLs } deriving (Eq, Show)

prog1 :: Programmer
prog1 = Programmer Linux Haskell

allOS :: [OSs]
allOS = [Linux, BSD, Mac, Windows]

allPL :: [PLs]
allPL = [Haskell, Agda, Idris, PureScript]

allProgrammer :: [Programmer]
allProgrammer = [(Programmer x y) | x <- allOS, y <- allPL]

-- deconstructing values
newtype Name' = Name' String deriving Show
newtype Acres = Acres Int deriving Show
data FarmerType = DairyFarmer | WheatFarmer | SoybeanFarmer deriving Show

data Farmer = Farmer Name Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

-- Bottom in record syntax
-- DONT put record type in sum type like this
data Automobile = Null
                | Car { make :: String
                      , model :: String
                      , year :: Integer }
                deriving (Eq, Show)
-- INSTEAD, separate the record before put it in the sum type
data Car' = Car' { make' :: String
                 , model' :: String
                 , year' :: String } deriving (Show, Eq)

data Automobile' = Null' | Automobile' Car' deriving (Show, Eq)
