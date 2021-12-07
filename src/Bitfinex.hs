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
  ) where

import Common
import Public.Ticker
import Authenticated.MarginInfo
import Authenticated.RetrievePositions
