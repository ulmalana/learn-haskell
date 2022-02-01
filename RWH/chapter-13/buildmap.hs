import qualified Data.Map as Map

al = [(1, "one"), (2, "two"), (3, "three"), (4, "four")]

-- all functions below perform the same task using different techniques

mapFromAL = Map.fromList al 

mapFold = foldl (\map (k,v) -> Map.insert k v map) Map.empty al 

-- order in this function may not be preserved
mapManual =  
    Map.insert 2 "two" .
    Map.insert 4 "four" .
    Map.insert 1 "one" .
    Map.insert 3 "three" $ Map.empty