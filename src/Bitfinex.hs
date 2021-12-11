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
  ) where

import Common
import Public.Ticker
import Authenticated.Wallets
import Authenticated.MarginInfo
import Authenticated.RetrievePositions
