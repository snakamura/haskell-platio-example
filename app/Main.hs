module Main ( main ) where

import Control.Concurrent.Async ( forConcurrently )
import Control.Exception ( SomeException
                         , try
                         )
import Data.Foldable ( for_ )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Options.Applicative as Opt
import System.IO ( stderr
                 , stdout
                 )

import qualified Platio.API as API
import qualified Platio.Record as R

data Options = Options
    { authorization :: Text
    }

optionsParser :: Opt.Parser Options
optionsParser = Options <$> Opt.strOption (Opt.short 'a' <> Opt.metavar "AUTHORIZATION" <> Opt.help "Authorization header")


main :: IO ()
main = do
    let opts = Opt.info optionsParser Opt.fullDesc
    options <- Opt.execParser opts
    api <- API.newAPI collectionUrl (authorization options)
    process api

collectionUrl :: Text
collectionUrl = "https://api.plat.io/v1/pwdhds3gsg5chpc6p4oes3af2ki/collections/t1c7d21c"


process :: API.API -> IO ()
process api = do
    records <- API.getLatestRecords api 10
    for_ records $ \record ->
        let name = maybe "" R.stringValue (R.name $ R.values record)
            age = maybe "" (T.pack . show @Int . floor . R.numberValue) (R.age $ R.values record)
        in T.hPutStrLn stdout $ "Id: " <> R.id record <> " Name: " <> name <> " Age: " <> age

    exceptions <- forConcurrently records $ \record ->
        let age = maybe 0 R.numberValue (R.age $ R.values record)
            values = R.Values Nothing (Just $ R.NumberValue $ age + 1)
        in try @SomeException $ API.updateRecord api (R.id record) values
    for_ (zip records exceptions) $ \(record, exception) ->
        case exception of
          Left e -> T.hPutStrLn stderr $ R.id record <> ": " <> T.pack (show e)
          Right _ -> pure ()
