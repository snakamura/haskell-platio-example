module Printer
    ( Printer
    , print
    , error
    , runPrinter
    ) where

import Data.Text ( Text )
import qualified Data.Text.IO as T
import Prelude hiding ( error
                      , print
                      )
import Polysemy ( Embed
                , Member
                , Sem
                , embed
                , interpret
                , makeSem
                )
import System.IO ( stderr
                 , stdout
                 )

data Printer m a where
    Print :: Text -> Printer m ()
    Error :: Text -> Printer m ()

makeSem ''Printer


runPrinter :: Member (Embed IO) r => Sem (Printer : r) a -> Sem r a
runPrinter = interpret \case
    Print text -> embed $ T.hPutStrLn stdout text
    Error text -> embed $ T.hPutStrLn stderr text
