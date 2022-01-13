data Profession = Fighter | Archer | Accountant

data Race = Human | Orc | Goblin

data PlayerCharacter = PlayerCharacter Race Profession

newtype CharList = CharList { getCharList :: [Char]} deriving (Eq, Show)

-- newtype is lazier than data
--data CoolBool = CoolBool { getCoolBool :: Bool }
newtype CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
-- try with > helloMe undefined
helloMe (CoolBool _) = "hello"
