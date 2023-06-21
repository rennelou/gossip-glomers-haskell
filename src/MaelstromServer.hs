{-# LANGUAGE DeriveGeneric #-}
module MaelstromServer (
    Message(..)
  , Body(..)
  , NodeData(..)
  , runMaelstrom ) where

import State

import System.IO (hPutStrLn, stderr)
import Data.Text
import Data.Aeson
import GHC.Generics
import qualified Data.Char               as C
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL

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

runMaelstrom :: (NodeData -> Message -> Message) -> IO ()
runMaelstrom clientHandler =
  let handler = createHandler clientHandler
  in do
    _ <- loop handler (NotInitialized)
    return ()

createHandler :: (NodeData -> Message -> Message) -> (Message -> Action Context Message)
createHandler f =
  \ message -> 
    Action (\ context ->
      case context of
        NotInitialized -> error "not initialized"
        Initialized nodeData -> State { state = context, content = f nodeData message } )

loop :: (Message -> Action Context Message) -> Context -> IO (Context)
loop handler context = 
  do
    line <- getLine
    log' ("Received: " ++ line)
    
    case eitherDecodeMessage line of 
      Left e -> 
        do
          log' e
          loop handler context
      Right message -> 
        let State { 
            state = newContext
          , content = responseMessage 
          } = run (handler message) context
        in do
          send responseMessage
          loop handler newContext

send :: Message -> IO ()
send responseMessage =
  do 
    log' ("Transmited: " ++ response)
    putStrLn response
  where
    response = encodeMessage responseMessage

log' :: String -> IO ()
log' str = hPutStrLn stderr str

eitherDecodeMessage :: String -> Either String Message
eitherDecodeMessage = eitherDecode  . TL.encodeUtf8 .TL.pack

encodeMessage :: Message -> String
encodeMessage = TL.unpack . TL.decodeUtf8 . encode
