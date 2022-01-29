data Ternary = Yes
             | No 
             | Unknown
             deriving (Eq, Show)

class Arbitrary a where
    arbitrary :: Gen a 

instance Arbitrary Ternary where
    arbitrary = elements [Yes, No, Unknown]

instance (Arbitrary a, Arbitrary b) => Arbitrary (a,b) where
    arbitrary = do 
        x <- arbitrary
        y <- arbitrary
        return (x,y)

elements :: [a] -> Gen a 
choose :: Random a => (a, a) -> Gen a 
oneof :: [Gen a] -> Gen a