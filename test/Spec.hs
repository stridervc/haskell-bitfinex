import Bitfinex

main :: IO ()
main = do
  let client = newBitfinexClient
  print =<< ticker client "tBTCUSD"
