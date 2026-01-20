#!/usr/bin/env run-script
module Application.Script.AgentSetup where

import Application.Script.Prelude
import Data.Aeson
import Data.Map qualified as M
import GHC.Generics
import Network.HTTP.Simple

data BinResponse = BinResponse
  { args :: Map String String
  }
  deriving (Show, Generic)

instance FromJSON BinResponse

-- >>> what "{\"args\": {\"asdf\": \"ok\"}, \"asdf\": \"ok\"}"
-- Just (BinResponse {args = fromList [("asdf","ok")]})
-- >>> what ""
-- Nothing
what x = decode x :: Maybe BinResponse

--- >>> wwhat "{\"args\": {\"asdf\": \"ok\"}, \"asdf\": \"ok\"}"
-- Success ()
wwhat x = fromJSON x :: Result BinResponse

-- >>> doSomething "{\"args\": {\"asdf\": \"ok\"}, \"asdf\": \"ok\"}"
-- Just "ok"
doSomething x
  | Just parsed <- decodedData = parsed.args M.!? "asdf"
  where
    decodedData = what x

run :: Script
run = do
  response <- httpJSON "http://httpbin.org/get?blah=ok" :: IO (Response BinResponse)
  let body = getResponseBody response
   in putStrLn $ show body.args
