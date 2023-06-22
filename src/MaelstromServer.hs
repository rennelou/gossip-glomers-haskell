{-# LANGUAGE DeriveGeneric #-}
module MaelstromServer (
    MaelstromMessage(..)
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


-------------------------- Maelstrom Server -------------------------------------------------

data MaelstromContext = NotInitialized | Initialized NodeData

data NodeData = NodeData {
  nodeId  :: Text,
  nodeIds :: [Text]
}

type ClientHandler = NodeData -> MaelstromMessage -> MaelstromMessage
type MaelstromHandler = MaelstromMessage -> State MaelstromContext (Either String MaelstromMessage)

runMaelstrom :: ClientHandler -> IO ()
runMaelstrom clientHandler =
  do
    _ <- loop (wrapperHandler clientHandler) (NotInitialized)
    return ()

loop :: MaelstromHandler -> MaelstromContext -> IO (MaelstromContext)
loop maelstromHandler context = 
  do
    line <- getLine
    log' ("Received: " ++ line)
    
    case eitherDecodeMessage line of 
      Left e -> 
        do
          log' e
          loop maelstromHandler context
      Right message -> 
        let (newContext, eitherMessage) = run (maelstromHandler message) context
        in
          case eitherMessage of
            Left e ->
              do
                log' e
                loop maelstromHandler newContext
            Right responseMessage ->
              do
                send responseMessage
                loop maelstromHandler newContext

wrapperHandler :: ClientHandler -> MaelstromHandler
wrapperHandler clientHandler message =
  do
    maelstromContext <- get ()
    case maelstromContext of
      NotInitialized ->
        
        case body message of
          Init _msgId _nodeId _nodeIds ->
            do
              set (Initialized NodeData { nodeId = _nodeId, nodeIds = _nodeIds })
              return (Right $ initOkMsg message _msgId)

          Init_Ok _ ->
            return (Left "Received an init_ok message")
          
          Error _ _ _ ->
            return (Left "Received an error message")
          
          Echo _msgId _ ->
            return (Right $ notInitializedErrorMsg message _msgId)
          
          Echo_Ok _ _ _ ->
            return (Left "Received an echo_ok message")

      Initialized nodeData -> return (Right $ clientHandler nodeData message)
  
initOkMsg :: MaelstromMessage -> Int -> MaelstromMessage
initOkMsg message msgIdMsg =
  MaelstromMessage {
      src = dest message
    , dest = src message
    , body = Init_Ok { in_reply_to = msgIdMsg } }

notInitializedErrorMsg :: MaelstromMessage -> Int -> MaelstromMessage
notInitializedErrorMsg message msgId =
  MaelstromMessage {
      src = dest message
    , dest = src message
    , body = Error { in_reply_to = msgId, code = 11, text = "Not Initialized" } }

send :: MaelstromMessage -> IO ()
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

eitherDecodeMessage :: String -> Either String MaelstromMessage
eitherDecodeMessage = Json.eitherDecode  . TL.encodeUtf8 .TL.pack

encodeMessage :: MaelstromMessage -> String
encodeMessage = TL.unpack . TL.decodeUtf8 . Json.encode

----------------------------------------------------------------------------------------

--------------------- Maelstrom Protocol Messages --------------------------------------

data MaelstromMessage = MaelstromMessage {
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

instance Json.FromJSON MaelstromMessage
instance Json.ToJSON MaelstromMessage

---------------------------------------------------------------------------------------------

------------------------- Client Events and Responses ---------------------------------------

-- * Insert Events, Responses and Parses Here *

---------------------------------------------------------------------------------------------

