{-# Language DeriveGeneric #-}

module Authenticated.RetrievePositions
  ( Position (..)
  , positions
  ) where

import Common
import Data.Aeson
import GHC.Generics
import Data.Maybe (fromJust)

newtype PositionRaw = PositionRaw [Value] deriving (Eq, Show, Generic)
instance FromJSON PositionRaw

data Position = Position
  { positionSymbol      :: String
  , positionStatus      :: String
  , positionAmount      :: Float
  , positionBasePrice   :: Float
  , positionFunding     :: Float
  , positionFundingType :: Float
  , positionPL          :: Float
  , positionPLPerc      :: Float
  , positionLiqPrice    :: Float
  , positionLeverage    :: Float
  , positionID          :: Float
  } deriving (Eq, Show)

fromRaw :: PositionRaw -> Position
fromRaw (PositionRaw v) = Position sym status amount bp funding ft pl plperc liq lev id'
  where decode' a = case fromJSON a of
                      Success a -> a
                      Error e   -> error e
        sym       = decode' $ head v
        status    = decode' $ v!!1
        amount    = decode' $ v!!2
        bp        = decode' $ v!!3
        funding   = decode' $ v!!4
        ft        = decode' $ v!!5
        pl        = decode' $ v!!6
        plperc    = decode' $ v!!7
        liq       = decode' $ v!!8
        lev       = decode' $ v!!9
        id'       = decode' $ v!!11

positions :: BitfinexClient -> IO [Position]
positions client = map fromRaw <$> queryBitfinexAuthenticated client "r/positions"
