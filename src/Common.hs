{-# LANGUAGE OverloadedStrings #-}

module Common
  ( Price
  , Percentage
  , BitfinexClient (..)
  , newBitfinexClient
  , newAuthenticatedBitfinexClient
  , queryBitfinexPublic
  , queryBitfinexAuthenticated
  ) where

import Data.Time
import Data.Aeson
import Network.HTTP.Simple
import Data.Maybe (fromJust)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (fromStrict)
import Data.Digest.Pure.SHA (hmacSha384)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Data.ByteString.Lazy.UTF8 (fromString)

type Price      = Float
type Percentage = Float

data BitfinexClient = BitfinexClient
  { _publicBaseUrl        :: String
  , _authenticatedBaseUrl :: String
  , _key                  :: Maybe ByteString
  , _secret               :: Maybe ByteString
  , _affiliate            :: Maybe ByteString
  }

newBitfinexClient :: BitfinexClient
newBitfinexClient = BitfinexClient "https://api-pub.bitfinex.com/v2/" "https://api.bitfinex.com" Nothing Nothing Nothing

newAuthenticatedBitfinexClient :: ByteString -> ByteString -> Maybe ByteString -> BitfinexClient
newAuthenticatedBitfinexClient key secret affiliate = newBitfinexClient
  { _key        = Just key
  , _secret     = Just secret
  , _affiliate  = affiliate
  }

queryBitfinexPublic :: (FromJSON a) => BitfinexClient -> String -> IO a
queryBitfinexPublic client endpoint = do
  url <- parseRequest $ _publicBaseUrl client <> endpoint
  return <$> getResponseBody =<< httpJSON url

queryBitfinexAuthenticated :: (FromJSON a) => BitfinexClient -> String -> IO a
queryBitfinexAuthenticated client endpoint = do
  {-
  now <- getPOSIXTime
  let nonce = show now
  -}
  now <- getCurrentTime
  let nonce = show $ floor $ 1e9 * nominalDiffTimeToSeconds (utcTimeToPOSIXSeconds now)
  let apipath = "/v2/auth/" <> endpoint
  let signature = "/api/" <> apipath <> nonce
  let signed = show $ hmacSha384 (fromStrict apisecret) (fromString signature)

  request' <- parseRequest $ _authenticatedBaseUrl client <> apipath
  let request = setRequestHeader "bfx-nonce" [ pack nonce ]
              $ setRequestHeader "bfx-apikey" [ apikey ]
              $ setRequestHeader "bfx-signature" [ pack signed ]
                request'

  return <$> getResponseBody =<< httpJSON request
  where apikey    = fromJust $ _key client
        apisecret = fromJust $ _secret client
