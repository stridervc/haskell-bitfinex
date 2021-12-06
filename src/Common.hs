module Common
  ( Price
  , Percentage
  , BitfinexClient (..)
  , newBitfinexClient
  , queryBitfinexPublic
  ) where

import Data.Aeson
import Network.HTTP.Simple

type Price      = Float
type Percentage = Float

data BitfinexClient = BitfinexClient
  { _publicBaseUrl        :: String
  , _authenticatedBaseUrl :: String
  }

newBitfinexClient :: BitfinexClient
newBitfinexClient = BitfinexClient "https://api-pub.bitfinex.com/v2/" "https://api.bitfinex.com"

queryBitfinexPublic :: (FromJSON a) => BitfinexClient -> String -> IO a
queryBitfinexPublic client endpoint = do
  url <- parseRequest $ _publicBaseUrl client ++ endpoint
  return <$> getResponseBody =<< httpJSON url
