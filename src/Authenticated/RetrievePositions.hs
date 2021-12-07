{-# Language DeriveGeneric #-}

module Authenticated.RetrievePositions
  ( positions
  ) where

import Common
import Data.Aeson
import GHC.Generics

newtype PositionRaw = PositionRaw [Value] deriving (Eq, Show, Generic)
instance FromJSON PositionRaw

positions :: BitfinexClient -> IO [PositionRaw]
positions client = queryBitfinexAuthenticated client "r/positions"
