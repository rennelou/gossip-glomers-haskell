{-# LANGUAGE DeriveGeneric #-}
module MaelstromServer (
    Message(..)
  , Body(..)
  , NodeData(..)
  , runMaelstrom ) where

import State

import System.IO (hPutStrLn, hFlush, stdout, stderr)
import Data.Text
import qualified Data.Aeson              as Json
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
  | Error   { in_reply_to :: Int, code :: Int, text :: Text }
  | Echo    { msg_id :: Int, echo :: Text }
  | Echo_Ok { msg_id :: Int, in_reply_to :: Int, echo :: Text }
  deriving (Generic, Show, Eq)

bodyJSONOptions :: Json.Options
bodyJSONOptions = 
  Json.defaultOptions {
     Json.sumEncoding = Json.TaggedObject { Json.tagFieldName = "type", Json.contentsFieldName = "contents" }
    ,Json.constructorTagModifier = fmap C.toLower
  }

instance Json.FromJSON Body where
  parseJSON = Json.genericParseJSON bodyJSONOptions

instance Json.ToJSON Body where
  toJSON = Json.genericToJSON bodyJSONOptions

instance Json.FromJSON Message
instance Json.ToJSON Message

data MaelstromContext = NotInitialized | Initialized NodeData

data NodeData = NodeData {
  nodeId  :: Text,
  nodeIds :: [Text]
}

runMaelstrom :: (NodeData -> Message -> Message) -> IO ()
runMaelstrom clientHandler =
  do
    _ <- loop handler (NotInitialized)
    return ()
  where
    handler = createHandler clientHandler

loop :: (Message -> State MaelstromContext Message) -> MaelstromContext -> IO (MaelstromContext)
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
        let (newContext, responseMessage) = run (handler message) context
        in do
          send responseMessage
          loop handler newContext

createHandler :: (NodeData -> Message -> Message) -> Message -> State MaelstromContext Message
createHandler f message =
  do
    maelstromContext <- get ()
    case maelstromContext of
      NotInitialized ->
        
        case body message of
          Init _msgId _nodeId _nodeIds ->
            do
              set (Initialized NodeData { nodeId = _nodeId, nodeIds = _nodeIds })
              return (initOkMsg (dest message) (src message) _msgId)
          Init_Ok _     -> error "Received an init_ok message"
          Error   _ _ _ -> error "Received an error message"
          Echo _msgId _ -> return (notInitializedErrorMsg (dest message) (src message) _msgId)
          Echo_Ok _ _ _ -> error "Received an echo_ok message"

      Initialized nodeData -> return (f nodeData message)
  
initOkMsg :: Text -> Text -> Int -> Message
initOkMsg src dest msgId =
  Message {
      src = src
    , dest = dest
    , body = Init_Ok { in_reply_to = msgId } }

notInitializedErrorMsg :: Text -> Text -> Int -> Message
notInitializedErrorMsg src dest msgId =
  Message {
      src = src
    , dest = dest
    , body = Error { in_reply_to = msgId, code = 11, text = "Not Initialized" } }

send :: Message -> IO ()
send responseMessage =
  do 
    putStrLn response
    hFlush stdout
    log' ("Sent: " ++ response)
  where
    response = encodeMessage responseMessage

log' :: String -> IO ()
log' str =
  do
    hPutStrLn stderr str
    hFlush stderr

eitherDecodeMessage :: String -> Either String Message
eitherDecodeMessage = Json.eitherDecode  . TL.encodeUtf8 .TL.pack

encodeMessage :: Message -> String
encodeMessage = TL.unpack . TL.decodeUtf8 . Json.encode
