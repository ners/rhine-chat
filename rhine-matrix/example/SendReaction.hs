module SendReaction where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.RFC8785
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HTTP
import Network.Matrix.Client
import Prelude
import Data.Text.IO qualified as Text
import Data.Text.Encoding qualified as Text
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Aeson.Encode.Pretty qualified as Aeson

aesonOptions :: Aeson.Options
aesonOptions = (aesonDrop 0 snakeCase){omitNothingFields = True}

--    "m.relates_to": {
--      "event_id": "$some_event_id",
--      "key": "👍",
--      "rel_type": "m.annotation"
--    }

data Annotation = Annotation
    { eventId :: Text
    , key :: Text
    }
    deriving stock (Generic)

instance FromJSON Annotation where
    parseJSON = Aeson.withObject "Annotation" \o -> do
        type' :: String <- o Aeson..: "rel_type"
        case type' of
            "m.annotation" -> Aeson.genericParseJSON aesonOptions (Aeson.toJSON o)
            _ -> mzero

instance ToJSON Annotation where
    toJSON ann =
        case Aeson.genericToJSON aesonOptions ann of
            Object o -> Object $ KeyMap.insert "rel_type" (Aeson.toJSON @String "m.annotation") o
            x -> x

data RelatesTo
    = RelAnnotation Annotation
    | Other Aeson.Object
    deriving stock (Generic)

instance FromJSON RelatesTo where
    parseJSON = Aeson.withObject "RelatesTo" \o -> do
        rel :: Object <- o Aeson..: "m.relates_to"
        (RelAnnotation <$> parseJSON (Object rel)) <|> pure (Other rel)

instance ToJSON RelatesTo where
    toJSON (RelAnnotation ann) = object ["m.relates_to" .= toJSON ann]
    toJSON (Other o) = object ["m.relates_to" .= toJSON o]

sendReaction :: ClientSession -> RoomID -> EventID -> Text -> Text -> MatrixIO EventID
sendReaction session (RoomID roomId) (EventID eventId) transactionId key = do
    request <- mkRequest session True $ "/_matrix/client/v3/rooms/" <> roomId <> "/send/m.reaction/" <> transactionId
    let relation = RelAnnotation Annotation{..}

    Text.putStrLn . Text.decodeUtf8 . LazyByteString.toStrict . Aeson.encodePretty $ relation
    doRequest session request{HTTP.method = "PUT", HTTP.requestBody = HTTP.RequestBodyLBS $ encodeCanonical relation}
