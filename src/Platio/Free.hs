module Platio.Free
    ( APIF
    , getLatestRecords
    , updateRecord
    , runAPIF
    ) where

import Control.Exception ( SomeException
                         , try
                         )
import Polysemy ( Embed
                , Member
                , Sem
                , makeSem
                , reinterpret2
                )
import Polysemy.Error ( Error
                      , fromEitherM
                      )
import Polysemy.Input ( Input
                      , input
                      )

import qualified Platio.API as API
import Platio.Record

data APIF m a where
    GetLatestRecords :: Int -> APIF m [Record]
    UpdateRecord :: RecordId -> Values -> APIF m ()

makeSem ''APIF


runAPIF :: Member (Embed IO) r => Sem (APIF : r) a -> Sem (Input API.API : Error SomeException : r) a
runAPIF = reinterpret2 \case
    GetLatestRecords count -> do
        api <- input
        fromEitherM $ try @SomeException $ API.getLatestRecords api count
    UpdateRecord recordId values -> do
        api <- input
        fromEitherM $ try @SomeException $ API.updateRecord api recordId values
