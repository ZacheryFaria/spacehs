module Application.STApi.Prelude
  ( module IHP.ControllerPrelude,
    module Generated.Types,
    module IHP.Prelude,
    mkUrl,
    makeRequest,
    dataMapper,
  )
where

import Data.ByteString.Char8 qualified as C
import Data.Text (stripSuffix)
import Generated.Types
import IHP.ControllerPrelude
import IHP.Prelude
import Network.HTTP
import Network.HTTP.Client.MultipartFormData (addPartHeaders)
import Network.HTTP.Simple
import Network.URI (URI, parseURI, relativeTo, uriToString)

-- >>> baseEndpoint
baseEndpoint :: [Char]
baseEndpoint = "https://api.spacetraders.io/v2/"

makeRequest :: [Char] -> [Char] -> Network.HTTP.Simple.Request
makeRequest url token = addRequestHeader "Authorization" headerValue request
  where
    fullUrl = mkUrl url
    request = parseRequest_ fullUrl
    headerValue = "Bearer " ++ token |> C.pack

mkUrl :: [Char] -> [Char]
mkUrl path = baseEndpoint ++ strippedPath
  where
    strippedPath = fromMaybe path $ stripPrefix "/" path

dataMapper =
  defaultOptions
    { fieldLabelModifier = mapper
    }
  where
    mapper "responseData" = "data"
    mapper s = s
