{-# LANGUAGE DeriveGeneric #-}
{-# Language OverloadedStrings #-}

module Authenticated.CancelOrder
  ( cancelOrder
  ) where

import Common
import Data.Aeson
import GHC.Generics

newtype CancelOrderResponseRaw = CancelOrderResponseRaw [Value] deriving (Eq, Show, Generic)
instance FromJSON CancelOrderResponseRaw

cancelOrder :: BitfinexClient -> Int -> IO CancelOrderResponseRaw
cancelOrder client orderID = queryBitfinexAuthenticatedWithBody client [("id", ParamInt orderID)] "w/order/cancel"
