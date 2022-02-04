import Control.Monad

data Context = Home | Mobile | Business deriving (Eq, Show)

type Phone = String

albulena = [(Home, "+123456")]
nils = [(Mobile, "+234563"), (Business, "+6454434")
        , (Home, "+34574"), (Business, "+989786")]
twalumba = [(Business, "+56785")]

onePersonalPhone :: [(Context, Phone)] -> Maybe Phone
onePersonalPhone ps = case lookup Home ps of
                        Nothing -> lookup Mobile ps
                        Just n -> Just n 

allBussinessPhones :: [(Context, Phone)] -> [Phone]
allBussinessPhones ps = map snd numbers
    where numbers = case filter (contextIs Business) ps of
                        [] -> filter (contextIs Mobile) ps 
                        ns -> ns
contextIs a (b, _) = a == b   

oneBusinessPhone :: [(Context, Phone)] -> Maybe Phone
oneBusinessPhone ps = lookup Business ps `mplus` lookup Mobile ps 

allPersonalPhones :: [(Context, Phone)] -> [Phone]
allPersonalPhones ps = map snd $ filter (contextIs Home) ps `mplus`
                                 filter (contextIs Mobile) ps  

lookupM :: (MonadPlus m, Eq a) => a -> [(a,b)] -> m b 
lookupM _ [] = mzero
lookupM k ((x,y):xys) | x == k = return y `mplus` lookupM k xys
                      | otherwise = lookupM k xys                                                                             