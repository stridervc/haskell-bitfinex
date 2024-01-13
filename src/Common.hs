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
  , ParamType (..)
  , stringify
  , OrderType (..)
  , orderTypeFromString
  ) where

import Data.Time
import Data.Aeson
import GHC.Generics
import Network.HTTP.Simple
import Data.Maybe (fromJust)
import Data.Digest.Pure.SHA (hmacSha384)
import Data.ByteString.UTF8 (fromString)
import Data.ByteString (ByteString, fromStrict, toStrict)
import Data.ByteString.Char8 (pack, unpack, intercalate)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import qualified Data.ByteString.Char8 as C8

type Price      = Float
type Percentage = Float
data OrderType  = MarginLimit | ExchangeLimit | MarginMarket | ExchangeMarket | MarginStop | ExchangeStop | MarginStopLimit
                | ExchangeStopLimit | MarginTrailingStop | ExchangeTrailingStop | MarginFOK | ExchangeFOK | MarginIOC | ExchangeIOC
                | UnknownOrderType
                deriving (Eq, Show)

data BitfinexClient = BitfinexClient
  { _publicBaseUrl        :: String
  , _authenticatedBaseUrl :: String
  , _key                  :: Maybe ByteString
  , _secret               :: Maybe ByteString
  , _affiliate            :: Maybe ByteString
  }

data ParamType = ParamString String | ParamFloat Float | ParamInt Int | ParamRaw ByteString deriving (Eq, Show)
type Params = [(ByteString, ParamType)]

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

-- the Bitfinex API doesn't like Aeson's ToJSON value when there's only one
-- it expects a structure with a single key : value
-- this function hopes to behave more like java's JSON.stringify
stringify :: Params -> ByteString
stringify []      = ""
stringify params  = "{" <> keyvalues <> "}"
  where keyvalues = intercalate "," $ map (\(k,v) -> "\"" <> k <> "\":" <> encode' v) params
        encode' (ParamString s) = toStrict $ encode s
        encode' (ParamFloat f)  = toStrict $ encode $ show f
        encode' (ParamInt i)    = toStrict $ encode i
        encode' (ParamRaw r)    = r

queryBitfinexAuthenticatedWithBody :: FromJSON a => BitfinexClient -> Params -> String -> IO a
queryBitfinexAuthenticatedWithBody client params endpoint = do
  now <- getCurrentTime
  let nonce     = show $ floor $ 1e6 * nominalDiffTimeToSeconds (utcTimeToPOSIXSeconds now)
  let apipath   = "/v2/auth/" <> endpoint
  let signature = "/api" <> apipath <> nonce <> unpack (stringify params)
  let signed    = show $ hmacSha384 (fromStrict apisecret) (fromStrict $ fromString signature)

  request' <- parseRequest $ "POST " <> _authenticatedBaseUrl client <> apipath
  let request = setRequestHeader "Content-Type" [ "application/json" ]
              $ setRequestHeader "bfx-nonce" [ C8.pack nonce ]
              $ setRequestHeader "bfx-apikey" [ apikey ]
              $ setRequestHeader "bfx-signature" [ C8.pack signed ]
              $ setRequestBodyLBS (fromStrict $ stringify params)
                request'

  return <$> getResponseBody =<< httpJSON request
  where apikey      = fromJust $ _key client
        apisecret   = fromJust $ _secret client
        affiliate   = show $ fromJust $ _affiliate client

queryBitfinexAuthenticated :: (FromJSON a) => BitfinexClient -> String -> IO a
queryBitfinexAuthenticated client = queryBitfinexAuthenticatedWithBody client []

orderTypeFromString :: String -> OrderType
orderTypeFromString "LIMIT"   = MarginLimit
orderTypeFromString "EXCHANGE LIMIT"          = ExchangeLimit
orderTypeFromString "MARKET"                  = MarginMarket
orderTypeFromString "EXCHANGE MARKET"         = ExchangeMarket
orderTypeFromString "STOP"                    = MarginStop
orderTypeFromString "EXCHANGE STOP"           = ExchangeStop
orderTypeFromString "STOP LIMIT"              = MarginStopLimit
orderTypeFromString "EXCHANGE STOP LIMIT"     = ExchangeStopLimit
orderTypeFromString "TRAILING STOP"           = MarginTrailingStop
orderTypeFromString "EXCHANGE TRAILING STOP"  = ExchangeTrailingStop
orderTypeFromString "FOK"                     = MarginFOK
orderTypeFromString "EXCHANGE FOK"            = ExchangeFOK
orderTypeFromString "IOC"                     = MarginIOC
orderTypeFromString "EXCHANGE IOC"            = ExchangeIOC
orderTypeFromString _                         = UnknownOrderType

