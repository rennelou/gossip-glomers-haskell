module MyLib (Message(..), Body(..), handler) where

import Data.Text
import Data.Aeson
import GHC.Generics

data Message = Message {
    src  :: Text,
    dest :: Text,
    body :: Body
} deriving (Generic, Show)

data Body = Body {
    type_text   :: Text,
    msg_id      :: Maybe Integer,
    in_reply_to :: Maybe Integer
} deriving (Generic, Show)

instance FromJSON MyLib.Body
instance ToJSON MyLib.Body

instance FromJSON MyLib.Message
instance ToJSON MyLib.Message

handler :: Message -> Message
handler = id
