{-# LANGUAGE OverloadedStrings #-}

module ScottyTest where

import Web.Scotty
import Data.Monoid (mconcat)

-- run this function in GHCi then visit http://localhost:3000/<word>
-- this <word> will be passed to this function and displayed with HTML script
-- below.
main = scotty 3000 $ do
    get "/:word" $ do
        beam <- param "word"
        html $ (mconcat ["<h1>Scotty, ", beam, " me up!</h1>"])
