{-# LANGUAGE DeriveGeneric #-}

module Authenticated.CancelOrder
  ( cancelOrder
  ) where

import Common
import Data.Aeson
import GHC.Generics

newtype CancelOrderResponseRaw = CancelOrderResponseRaw [Value] deriving (Eq, Show, Generic)
instance FromJSON CancelOrderResponseRaw

newtype CancelOrderID = CancelOrderID { id :: Int } deriving (Eq, Show, Generic)
instance ToJSON CancelOrderID

cancelOrder :: BitfinexClient -> Int -> IO CancelOrderResponseRaw
cancelOrder client orderID = queryBitfinexAuthenticatedWithBody client (Just $ CancelOrderID orderID) "w/order/cancel"
