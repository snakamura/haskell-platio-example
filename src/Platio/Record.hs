module Platio.Record
    ( RecordId
    , Record(..)
    , Values(..)
    , StringValue(..)
    , NumberValue(..)
    ) where

import Data.Aeson ( (.=)
                  , (.:?)
                  )
import qualified Data.Aeson as JSON
import Data.Maybe ( catMaybes )
import Data.Text ( Text )
import GHC.Generics ( Generic )

type RecordId = Text


data Record = Record
    { id :: RecordId
    , values :: Values
    } deriving (Show, Generic)

instance JSON.FromJSON Record
instance JSON.ToJSON Record


data Values = Values
    { name :: Maybe StringValue
    , age :: Maybe NumberValue
    } deriving Show

nameColumnId, ageColumnId :: Text
nameColumnId = "cd33ed98"
ageColumnId = "ce0f2361"

instance JSON.FromJSON Values where
    parseJSON = JSON.withObject "Values" $ \o ->
        Values <$> o .:? nameColumnId
               <*> o .:? ageColumnId

instance JSON.ToJSON Values where
    toJSON (Values name age) =
        JSON.object $ catMaybes [ (nameColumnId .=) <$> name
                                , (ageColumnId .=) <$> age
                                ]


data StringValue = StringValue
    { stringValue :: Text
    } deriving (Show, Generic)

instance JSON.FromJSON StringValue where
    parseJSON = let options = JSON.defaultOptions { JSON.fieldLabelModifier = const "value" }
                in JSON.genericParseJSON options

instance JSON.ToJSON StringValue where
    toJSON (StringValue stringValue) =
        JSON.object [ "type" .= ("String" :: Text)
                    , "value" .= stringValue
                    ]


data NumberValue = NumberValue
    { numberValue :: Double
    } deriving (Show, Generic)

instance JSON.FromJSON NumberValue where
    parseJSON = let options = JSON.defaultOptions { JSON.fieldLabelModifier = const "value" }
                in JSON.genericParseJSON options

instance JSON.ToJSON NumberValue where
    toJSON (NumberValue numberValue) =
        JSON.object [ "type" .= ("Number" :: Text)
                    , "value" .= numberValue
                    ]
