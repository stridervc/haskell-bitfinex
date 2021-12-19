{-# LANGUAGE DeriveGeneric #-}
{-# Language OverloadedStrings #-}

module Authenticated.SubmitOrder
  ( submitOrder
  ) where

import Common
import Data.Aeson
import GHC.Generics
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)

newtype ResponseRaw = ResponseRaw [Value] deriving (Eq, Show, Generic)
instance FromJSON ResponseRaw

type OrderType  = String
type Symbol     = String
type Amount     = Float

submitOrder :: BitfinexClient -> OrderType -> Symbol -> Amount -> Price -> IO ResponseRaw
submitOrder client ordertype symbol amount price = queryBitfinexAuthenticatedWithBody client
  [ ("type",    ordertype)
  , ("symbol",  symbol)
  , ("price",   show price)
  , ("amount",  show amount)
  , ("flags",   show 4096) -- post only
  , ("meta",    unpack (stringify [("aff_code", unpack aff_code)]))
  ] "w/order/submit"
  where Just aff_code = _affiliate client
