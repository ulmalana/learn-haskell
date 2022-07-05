module LiftedOr ((<||>)) where

import Control.Applicative (liftA2)

(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = liftA2 (||)

-- ordinary or
-- 
-- > True || False
-- True
--
-- build boolean function
-- 
-- > let f 9001 = True; f _ = False
-- > let g 42 = True; g _ = False
--
-- composing f and g with ordinary or
-- > (\n -> f n || g n) 0
-- False
-- > (\n -> f n || g n) 9001
-- True
--
-- composing f and g with <||>. we dont need to pass the argument.
-- > (f <||> g) 0
-- False
-- > (f <||> g) 9001
-- True

