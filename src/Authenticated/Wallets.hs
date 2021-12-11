{-# LANGUAGE DeriveGeneric #-}

module Authenticated.Wallets
  ( Wallet (..)
  , wallets
  ) where

import Common
import Data.Aeson
import GHC.Generics

newtype WalletRaw = WalletRaw [Value] deriving (Eq, Show, Generic)
instance FromJSON WalletRaw

data Wallet = Wallet
  { walletType              :: String
  , walletCurrency          :: String
  , walletBalance           :: Float
  , walletUnsettledInterest :: Float
  , walletAvailableBalance  :: Float
  , walletLastChange        :: String
  } deriving (Eq, Show)

fromRaw :: WalletRaw -> Wallet
fromRaw (WalletRaw v) = Wallet wtype curr bal ui ab lc
  where decode' a = case fromJSON a of
                      Success a -> a
                      Error e   -> error e
        wtype     = decode' $ head v
        curr      = decode' $ v!!1
        bal       = decode' $ v!!2
        ui        = decode' $ v!!3
        ab        = decode' $ v!!4
        lc        = decode' $ v!!5

wallets :: BitfinexClient -> IO [Wallet]
wallets client = map fromRaw <$> queryBitfinexAuthenticated client "r/wallets"
