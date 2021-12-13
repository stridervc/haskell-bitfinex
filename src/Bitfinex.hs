module Bitfinex
  ( BitfinexClient (..)
  , newBitfinexClient
  , newAuthenticatedBitfinexClient
  , Ticker (..)
  , ticker
  , MarginInfo (..)
  , marginInfo
  , Position (..)
  , positions
  , Wallet (..)
  , wallets
  , Order (..)
  , retrieveOrders
  ) where

import Common
import Public.Ticker
import Authenticated.Wallets
import Authenticated.MarginInfo
import Authenticated.RetrieveOrders
import Authenticated.RetrievePositions
