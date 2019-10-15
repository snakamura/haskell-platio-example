module Platio.API
    ( API
    , newAPI
    , getLatestRecords
    , updateRecord
    ) where

import Control.Exception ( Exception )
import Control.Monad.Catch ( throwM )
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BL
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics ( Generic )
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP

import Platio.Record

data API = API
    { manager :: HTTP.Manager
    , collectionUrl :: Text
    , authorization :: Text
    }

newAPI :: Text -> Text -> IO API
newAPI collectionUrl authorization = do
    manager <- HTTP.newManager HTTP.tlsManagerSettings
    pure $ API manager collectionUrl authorization


getLatestRecords :: API -> Int -> IO [Record]
getLatestRecords api count = do
    let url = collectionUrl api <> "/records?limit=" <> T.pack (show count)
    req <- HTTP.parseUrlThrow $ T.unpack url
    res <- sendRequest api req
    case JSON.eitherDecode $ HTTP.responseBody res of
      Left e -> throwM $ APIException $ T.pack e
      Right records -> pure records


updateRecord :: API -> RecordId -> Values -> IO ()
updateRecord api recordId values = do
    let url = collectionUrl api <> "/records/" <> recordId
        body = JSON.encode $ UpdateRecord values False
    initialReq <- HTTP.parseUrlThrow $ T.unpack url
    let req = initialReq { HTTP.method = "PUT"
                         , HTTP.requestBody = HTTP.RequestBodyLBS body
                         }
    _ <- sendRequest api req
    pure ()

data UpdateRecord = UpdateRecord
    { values :: Values
    , replace :: Bool
    } deriving (Show, Generic)

instance JSON.ToJSON UpdateRecord


sendRequest :: API -> HTTP.Request -> IO (HTTP.Response BL.ByteString)
sendRequest api req = do
    let headers = [ ("Authorization", T.encodeUtf8 (authorization api))
                  , ("Content-Type", "application/json")
                  ]
    HTTP.httpLbs req { HTTP.requestHeaders = headers } (manager api)


data APIException = APIException Text deriving Show

instance Exception APIException
