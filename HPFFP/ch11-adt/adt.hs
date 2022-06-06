data Price = Price Integer deriving (Show, Eq)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = Garuda | CityLinx | Batix deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar vh = 
    case vh of
        Car _ _ -> True
        Plane _ -> False

isPlane :: Vehicle -> Bool
isPlane vh = 
    case vh of
        Plane _ -> True
        Car _ _ -> False

areCars :: [Vehicle] -> [Bool]
areCars vhs = map isCar vhs
