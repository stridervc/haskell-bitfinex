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
  , submitOrder
  , Trade (..)
  , trades
  ) where

import Common
import Public.Ticker
import Authenticated.Trades
import Authenticated.Wallets
import Authenticated.MarginInfo
import Authenticated.CancelOrder
import Authenticated.SubmitOrder
import Authenticated.RetrieveOrders
import Authenticated.RetrievePositions
