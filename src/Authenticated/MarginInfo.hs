{-# LANGUAGE DeriveGeneric #-}

module Authenticated.MarginInfo
  ( MarginInfo (..)
  , marginInfo
  ) where

import Common
import Data.Aeson
import GHC.Generics

newtype MarginInfo = MarginInfo ([String], [Float]) deriving (Eq, Show, Generic)

instance FromJSON MarginInfo

marginInfo :: BitfinexClient -> IO MarginInfo
marginInfo client = queryBitfinexAuthenticated client "r/info/margin/tBTCUSD"
