module Main ( main ) where

import Control.Exception ( SomeException )
import Data.Foldable ( for_ )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Traversable ( for )
import qualified Options.Applicative as Opt
import Polysemy ( Members
                , Sem
                , runM
                )
import Polysemy.Async ( Async
                      , async
                      , asyncToIO
                      , await
                      )
import Polysemy.Error ( runError )
import Polysemy.Input ( runInputConst )

import qualified Platio.API as API
import qualified Platio.Free as F
import qualified Platio.Record as R
import qualified Printer as P

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
    r <- runM $ P.runPrinter $ runError $ runInputConst api $ F.runAPIF $ asyncToIO $ processM
    case r of
      Left e -> print e
      Right _ -> pure ()

collectionUrl :: Text
collectionUrl = "https://api.plat.io/v1/pwdhds3gsg5chpc6p4oes3af2ki/collections/t1c7d21c"


processM :: Members '[ F.APIF, P.Printer, Async ] r => Sem r ()
processM = do
    records <- F.getLatestRecords 10
    for_ records $ \record ->
        let name = maybe "" R.stringValue (R.name $ R.values record)
            age = maybe "" (T.pack . show @Int . floor . R.numberValue) (R.age $ R.values record)
        in P.print $ "Id: " <> R.id record <> " Name: " <> name <> " Age: " <> age

    asyncs <- for records $ \record ->
        let age = maybe 0 R.numberValue (R.age $ R.values record)
            values = R.Values Nothing (Just $ R.NumberValue $ age + 1)
        in async $ runError @SomeException $ F.updateRecord (R.id record) values
    for_ (zip records asyncs) $ \(record, async) -> do
        r <- await async
        case r of
          Just (Left e) -> P.error $ R.id record <> ": " <> T.pack (show e)
          Just (Right _) -> pure ()
          Nothing -> pure ()
