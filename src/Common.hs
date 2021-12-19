{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Common
  ( Price
  , Percentage
  , BitfinexClient (..)
  , newBitfinexClient
  , newAuthenticatedBitfinexClient
  , queryBitfinexPublic
  , queryBitfinexAuthenticatedWithBody
  , queryBitfinexAuthenticated
  ) where

import GHC.Generics
import Data.Time
import Data.Aeson
import Network.HTTP.Simple
import Data.Maybe (fromJust)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (fromStrict)
import Data.Digest.Pure.SHA (hmacSha384)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Data.ByteString.Lazy.UTF8 (fromString, toString)

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

newtype AffCode = AffCode { aff_code :: String } deriving (Generic)
instance ToJSON AffCode
newtype AffiliateJSON = AffiliateJSON { meta :: AffCode } deriving (Generic)
instance ToJSON AffiliateJSON

queryBitfinexAuthenticatedWithBody :: (FromJSON a, ToJSON b) => BitfinexClient -> b -> String -> IO a
queryBitfinexAuthenticatedWithBody client body endpoint = do
  now <- getCurrentTime
  let nonce     = show $ floor $ 1e6 * nominalDiffTimeToSeconds (utcTimeToPOSIXSeconds now)
  let apipath   = "/v2/auth/" <> endpoint
  let signature = "/api" <> apipath <> nonce <> toString (encode body)
  let signature = "/api" <> apipath <> nonce <> ""
  let signed    = show $ hmacSha384 (fromStrict apisecret) (fromString signature)

  putStrLn $ "DBG: " <> toString (encode body)

  request' <- parseRequest $ "POST " <> _authenticatedBaseUrl client <> apipath
  let request = setRequestHeader "Content-Type" [ "application/json" ]
              $ setRequestHeader "bfx-nonce" [ pack nonce ]
              $ setRequestHeader "bfx-apikey" [ apikey ]
              $ setRequestHeader "bfx-signature" [ pack signed ]
              $ setRequestBody ""
              -- setRequestBodyLBS (encode body)
              -- setRequestBodyJSON (AffiliateJSON $ AffCode affiliate)
                request'

  return <$> getResponseBody =<< httpJSON request
  where apikey    = fromJust $ _key client
        apisecret = fromJust $ _secret client
        affiliate = show $ fromJust $ _affiliate client

queryBitfinexAuthenticated :: (FromJSON a) => BitfinexClient -> String -> IO a
queryBitfinexAuthenticated client = queryBitfinexAuthenticatedWithBody client ("" :: String)
