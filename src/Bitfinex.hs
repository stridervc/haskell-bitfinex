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
  , cancelOrder
  ) where

import Common
import Public.Ticker
import Authenticated.Wallets
import Authenticated.MarginInfo
import Authenticated.CancelOrder
import Authenticated.RetrieveOrders
import Authenticated.RetrievePositions
