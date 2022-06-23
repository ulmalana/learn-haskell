-- Ignoring possibilities

-- without fmap
incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing = Nothing

-- with fmap
incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+1) m

incMaybe' :: Num a => Maybe a -> Maybe a
incMaybe' = fmap (+1)

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe s = fmap show s

showMaybe' :: Show a => Maybe a -> Maybe String
showMaybe' = fmap show

-- lifted version. look at the type declaration
liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

-- exercise
data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers a) = Yeppers (f a)
    
