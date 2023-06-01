{-# LANGUAGE DeriveGeneric #-}
module MyLib (Message(..), Body(..), handler) where

import Data.Text
import Data.Aeson
import GHC.Generics

data Message = Message {
    src  :: Text,
    dest :: Text,
    body :: Body
} deriving (Generic, Show, Eq)

data Body =
    Init    { msg_id :: Int, node_id :: Text, node_ids :: [Text] }
  | Init_Ok { in_reply_to :: Int }
  | Echo    { msg_id :: Int, echo :: Text }
  | Echo_Ok { msg_id :: Int, in_reply_to :: Int, echo :: Text }
  deriving (Generic, Show, Eq)

instance FromJSON Body where
instance ToJSON Body

instance FromJSON Message
instance ToJSON Message

handler :: Message -> Message
handler = id
