{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Concurrent.STM 
import Control.Monad

data Item = Scroll
          | Wand
          | Banjo
            deriving (Eq, Ord, Show)

newtype Gold = Gold Int deriving (Eq, Ord, Show, Num)
newtype HitPoint = HitPoint Int deriving (Eq, Ord, Show, Num)

type Inventory = TVar [Item]
type Health = TVar HitPoint
type Balance = TVar Gold

data Player = Player {
    balance :: Balance,
    health :: Health,
    inventory :: Inventory
}

basicTransfer :: Gold -> Balance -> Balance -> STM ()
basicTransfer qty fromBal toBal = do 
    fromQty <- readTVar fromBal
    toQty <- readTVar toBal
    writeTVar fromBal (fromQty - qty)
    writeTVar toBal (toQty + qty)

transferTest = do 
    alice <- newTVar (12::Gold)
    bob <- newTVar 4
    basicTransfer 3 alice bob
    liftM2 (,) (readTVar alice) (readTVar bob)

removeInv :: Eq a => a -> [a] -> Maybe [a]
removeInv x xs =
    case takeWhile (/= x) xs of
        (_:ys)  -> Just ys
        []      -> Nothing

maybeGiveItem :: Item -> Inventory -> Inventory -> STM Bool
maybeGiveItem item fromInv toInv = do 
    fromList <- readTVar fromInv
    case removeInv item fromList of
        Nothing -> return False
        Just newList -> do 
            writeTVar fromInv newList
            destItems <- readTVar toInv
            writeTVar toInv (item:destItems)
            return True

maybeSellItem :: Item -> Gold -> Player -> Player -> STM Bool
maybeSellItem item price buyer seller = do 
    given <- maybeGiveItem item (inventory seller) (inventory buyer)
    if given
        then do 
            basicTransfer price (balance buyer) (balance seller)
            return True
        else return False

giveItem :: Item -> Inventory -> Inventory -> STM ()
giveItem item fromInv toInv = do 
    fromList <- readTVar fromInv
    case removeInv item fromList of
        Nothing -> retry
        Just newList -> do 
            writeTVar fromInv newList
            readTVar toInv >>= writeTVar toInv . (item :)

transfer :: Gold -> Balance -> Balance -> STM ()
transfer qty fromBal toBal = do 
    fromQty <- readTVar fromBal
    when (qty > fromQty) $
        retry
    writeTVar fromBal (fromQty - qty)
    readTVar toBal >>= writeTVar toBal . (qty +)

sellItem :: Item -> Gold -> Player -> Player -> STM ()
sellItem item price buyer seller = do 
    giveItem item (inventory seller) (inventory buyer)
    transfer price (balance buyer) (balance seller)

crummyList :: [(Item, Gold)] -> Player -> Player -> STM (Maybe (Item, Gold))
crummyList list buyer seller = go list 
  where go [] = return Nothing
        go (this@(item,price):rest) = do 
            sellItem item price buyer seller
            return (Just this)
          `orElse`
            go rest

maybeSTM :: STM a -> STM (Maybe a)
maybeSTM m = (Just `liftM` m) `orElse` return Nothing

shoppingList :: [(Item, Gold)] -> Player -> Player -> STM (Maybe (Item, Gold))
shoppingList list buyer seller = maybeSTM . msum $ map sellOne list 
  where sellOne this@(item,price) = do 
        sellItem item price buyer seller
        return this

maybeM :: MonadPlus m => m a -> m (Maybe a)
maybeM m = (Just `liftM` m) `mplus` return Nothing

bogusTransfer qty fromBal toBal = do
    fromQty <- atomically $ readTVar fromBal
    -- window of inconsistency
    toQty <- atomically $ readTVar toBal
    atomically $ writeTVar fromBal (fromQty - qty)
    -- window of inconsistency
    atomically $ writeTVar toBal (toQty + qty)

bogusSale :: Item -> Gold -> Player -> Player -> IO ()
bogusSale item price buyer seller = do
    atomically $ giveItem item (inventory seller) (inventory buyer)
    bogusTransfer price (balance buyer) (balance seller)

newPlayer :: Gold -> HitPoint -> [Item] -> STM Player
newPlayer balance health inventory =
    Player `liftM` newTVar balance
            `ap`    newTVar health
            `ap`    newTVar inventory

populateWorld :: STM [Player]
populateWorld = sequence [  newPlayer 20 20 [Wand, Banjo],
                            newPlayer 10 12 [Scroll] ]

consistentBalance :: [Player] -> STM (STM ())
consistentBalance players = do 
    initialTotal <- totalBalance
    return $ do 
        curTotal <- totalBalance
        when (curTotal /= initialTotal) $
            error "incosistent global balance"
  where totalBalance = foldM addBalance 0 players
        addBalance a b = (a+) `liftM` readTVar (balance b)


tryBogusSale = do 
    players@(alice:bob:_) <- atomically populateWorld
    atomically $ alwaysSucceeds =<< consistentBalance players
    bogusSale Wand 5 alice bob