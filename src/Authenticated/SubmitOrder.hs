{-# LANGUAGE DeriveGeneric #-}
{-# Language OverloadedStrings #-}

module Authenticated.SubmitOrder
  ( submitOrder
  ) where

import Common
import Data.Aeson
import GHC.Generics
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)

newtype ResponseRaw = ResponseRaw [Value] deriving (Eq, Show, Generic)
instance FromJSON ResponseRaw

type OrderTypeStr = String
type Symbol       = String
type Amount       = Float

submitOrder :: BitfinexClient -> OrderTypeStr -> Symbol -> Amount -> Price -> IO ResponseRaw
submitOrder client ordertype symbol amount price = queryBitfinexAuthenticatedWithBody client
  [ ("type",    ParamString ordertype)
  , ("symbol",  ParamString symbol)
  , ("price",   ParamFloat price)
  , ("amount",  ParamFloat amount)
  , ("flags",   ParamInt 4096) -- post only
  , ("meta",    ParamRaw meta)
  ] "w/order/submit"
  where Just aff_code = _affiliate client
        meta          = "{\"aff_code\":\"" <> aff_code <> "\"}"
