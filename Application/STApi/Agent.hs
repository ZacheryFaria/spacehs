module Application.STApi.Agent where

import Application.STApi.Prelude
import Application.STApi.Types
import Control.Monad
import Data.Aeson
import Data.Map qualified as M
import GHC.Generics
import Network.HTTP.Simple

data RegisterRequest = RegisterRequest
  { symbol :: String,
    faction :: String
  }
  deriving (Show, Generic)

instance ToJSON RegisterRequest

data RegisterResponseData = RegisterResponseData
  { token :: String,
    agent :: Agent,
    contract :: Contract,
    ships :: [Ship]
  }
  deriving (Show, Generic)

instance FromJSON RegisterResponseData

data RegisterResponse = RegisterResponse
  { responseData :: RegisterResponseData
  }
  deriving (Show, Generic)

instance FromJSON RegisterResponse where
  parseJSON = genericParseJSON dataMapper

registerAgent :: RegisterRequest -> IO RegisterResponse
registerAgent req = do
  response <- httpJSON (setRequestBodyJSON req request) :: IO (Response RegisterResponse)
  return $ getResponseBody response
  where
    request =
      makeRequest "/register" "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZGVudGlmaWVyIjoiY203NGE1cjhlMDAyM3RjMGo0MXY2bWl2MCIsInZlcnNpb24iOiJ2Mi4zLjAiLCJpYXQiOjE3NjY4MDYwNzgsInN1YiI6ImFjY291bnQtdG9rZW4ifQ.vJz7YvsgQFvlqT-lUBM25z60ki_JNAC5POQi0tv_qK4bmxR3zgj1FtcNoxWVJ3yrmudIBhNPxOdLUov-Igk1yG-yIL7nefrhaJzrLW3Te806XycPsxX-9nLqDhOeWBAlHpZrZMIoL_K5m9s-abwLPq5bSVn7MfHZqTtEkjo-yzr7sg3sbCadx6Hc4t3ipQfZe0FAXGKkRmRe1Y9oyMm1cUjwF0xYszVRnJOkJcm_m5ocV0y5B761t4XODcbqL97QRZFEqOnB7NZm9sboY3QX1QsajKGxVihtWV7Eg6Z8Huk_djiyhdyIlgCilLo1h1o0wO7xgn02uQpChwgUsgZSHyxNr7Kgoxn4JGioW21oPgG1widlgJkygosvOG1TFi2F7xjiEE5pixp537lMfrDpmCSdv-7_9bpz9cZ3kMH4eGaU4rxhOn-GBR1n0f7X4RPG1RKaz2BiY2DdF101QMlgPuIFO_fGjdgdKSNMvfXy6yeZB_8N70Nu2DgisDecSb2lTBmkhe6Rfxl5OOC3Azryu4vn2DRrJwY3-DnGCi4RpRRkDtSY-L5DZibaMxrQY5GtWNjlFSfPQMG19gTupnDrN5058ClWHa_qtOkQUriH3yzzFEL6HMv0abZL3S10fozu2qBRyJRHkRhokfbWqkjfLR1vz99kNccwafuKg9w29Ds"
        |> setRequestMethod "POST"
