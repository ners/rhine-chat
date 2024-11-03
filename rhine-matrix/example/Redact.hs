module Redact where

import Data.Aeson
import Data.Aeson.RFC8785
import Data.Text (Text)
import Network.HTTP.Client qualified as HTTP
import Network.Matrix.Client
import Prelude

sendRedaction :: ClientSession -> RoomID -> EventID -> Text -> MatrixIO ()
sendRedaction session (RoomID roomId) (EventID eventId) transactionId = do
    request <- mkRequest session True $ "/_matrix/client/v3/rooms/" <> roomId <> "/redact/" <> eventId <> "/" <> transactionId
    doRequest session request{HTTP.method = "PUT", HTTP.requestBody = HTTP.RequestBodyLBS $ encodeCanonical (object [])}
