{-# LANGUAGE DeriveGeneric #-}

module Authenticated.MarginInfo
  ( MarginInfo (..)
  , marginInfo
  ) where

import Common
import Data.Aeson
import GHC.Generics

data MarginInfo = MarginInfo
  { infoType  :: String
  , symbol    :: String
  , values    :: [Float]
  } deriving (Eq, Show, Generic)

instance FromJSON MarginInfo

marginInfo :: BitfinexClient -> IO MarginInfo
marginInfo client = queryBitfinexAuthenticated client "r/info/margin/tBTCUSD"
