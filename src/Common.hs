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

import Data.Time
import Data.Aeson
import GHC.Generics
import Network.HTTP.Simple
import Data.Maybe (fromJust)
import Data.ByteString.Char8 (pack)
import Data.Digest.Pure.SHA (hmacSha384)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.ByteString.Lazy.Char8 (unpack, intercalate)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)

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

-- the Bitfinex API doesn't like Aeson's ToJSON value when there's only one
-- it expects a structure with a single key : value
-- this function hopes to behave more like java's JSON.stringify
stringify :: ToJSON a => [(ByteString,a)] -> ByteString
stringify []      = ""
stringify params  = "{" <> keyvalues <> "}"
  where keyvalues = intercalate "," $ map (\(k,v) -> "\"" <> k <> "\":" <> encode v) params

queryBitfinexAuthenticatedWithBody :: (FromJSON a, ToJSON b) => BitfinexClient -> [(ByteString,b)] -> String -> IO a
queryBitfinexAuthenticatedWithBody client params endpoint = do
  now <- getCurrentTime
  let nonce     = show $ floor $ 1e6 * nominalDiffTimeToSeconds (utcTimeToPOSIXSeconds now)
  let apipath   = "/v2/auth/" <> endpoint
  let signature = "/api" <> apipath <> nonce <> unpack (stringify params)
  let signed    = show $ hmacSha384 apisecret (fromString signature)

  putStrLn $ "DBG: body = " <> unpack (stringify params)
  putStrLn ""

  request' <- parseRequest $ "POST " <> _authenticatedBaseUrl client <> apipath
  let request = setRequestHeader "Content-Type" [ "application/json" ]
              $ setRequestHeader "bfx-nonce" [ pack nonce ]
              $ setRequestHeader "bfx-apikey" [ toStrict apikey ]
              $ setRequestHeader "bfx-signature" [ pack signed ]
              $ setRequestBodyLBS (stringify params)
                request'

  return <$> getResponseBody =<< httpJSON request
  where apikey      = fromJust $ _key client
        apisecret   = fromJust $ _secret client
        affiliate   = show $ fromJust $ _affiliate client

queryBitfinexAuthenticated :: (FromJSON a) => BitfinexClient -> String -> IO a
queryBitfinexAuthenticated client = queryBitfinexAuthenticatedWithBody client ([] :: [(ByteString,Int)])
