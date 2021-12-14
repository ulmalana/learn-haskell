-- type and type classes

module Shapes
( Point(..)
, Shape(..)
, surface
, nudge
, baseCircle
, baseReact
) where

-- this Shape type consists of two constructors
-- - Circle : first two fields are the coordinates, the last one is radius
-- - Rectangle : first two are upper left corner coordinate, the last two are lower left corner
-- deriving (Show) for displaying in terminal
-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

-- intermediate data type
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- surface :: Shape -> Float
-- surface (Circle _ _ r) = pi * r ^ 2
-- surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2) ) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r 
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- auxilliary functions that create shapes of some size at the zero coordinates
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r 

baseReact :: Float -> Float -> Shape
baseReact width height = Rectangle (Point 0 0) (Point width height)

----------

data Person = Person String String Int Float String String deriving (Show)


firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname

lastName :: Person -> String
lastName (Person _ lastname  _ _ _ _) = lastname

age :: Person -> Int
age (Person _ _ age _ _ _) = age

height :: Person -> Float
height (Person _ _ _ height _ _) = height

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ number _) = number 

flavor :: Person -> String
flavor (Person _ _ _ _ _ flavor) = flavor

-- record syntax
data Person2 = Person2 { firstName2 :: String
                       , lastName2 :: String
                       , age2 :: Int
                       , height2 :: Float
                       , phoneNumber2 :: String
                       , flavor2 :: String
                       } deriving (Show)

-- type constructor with record syntax

--- prefer this one
data Car = Car { company :: String
                , model :: String 
                , year :: Int
                } deriving (Show, Eq, Read)

-- instead of this
-- data Car a b c = Car { company :: a  
--                      , model :: b  
--                      , year :: c   
--                      } deriving (Show) 

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y 

-- tellCar :: (Show a) => Car String String a -> String  
-- tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y 

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t 
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n 

-- derived instances

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum) 