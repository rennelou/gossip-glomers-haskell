{-# LANGUAGE DeriveGeneric #-}
module MaelstromServer (
    Message(..)
  , Body(..)
  , Context
  , NodeData(..)
  , Action
  , createHandler ) where

import State

import Data.Text
import Data.Aeson
import GHC.Generics
import qualified Data.Char as C

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

bodyJSONOptions :: Options
bodyJSONOptions = 
  defaultOptions {
     sumEncoding = TaggedObject { tagFieldName = "type", contentsFieldName = "contents" }
    ,constructorTagModifier = fmap C.toLower
  }

instance FromJSON Body where
  parseJSON = genericParseJSON bodyJSONOptions

instance ToJSON Body where
  toJSON = genericToJSON bodyJSONOptions

instance FromJSON Message
instance ToJSON Message

data Context = NotInitialized | Initialized NodeData

data NodeData = NodeData {
  nodeId  :: Int,
  nodeIds :: [Int]
}

createHandler :: (NodeData -> Message -> Message) -> (Message -> Action Context Message)
createHandler f =
  \ message -> 
    Action (\ context ->
      case context of
        NotInitialized -> error "not initialized"
        Initialized nodeData -> State { state = context, content = f nodeData message } )
