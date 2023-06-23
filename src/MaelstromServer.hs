{-# LANGUAGE DeriveGeneric #-}
module MaelstromServer (
    MaelstromMessage(..)
  , Body(..)
  , NodeData(..)
  , MaelstromContext(NotInitialized)
  , Event(..)
  , Response(..)
  , runMaelstrom
  , createMaelstromServer ) where

import State

import System.IO (hPutStrLn, hFlush, stdout, stderr)
import Data.Text
import qualified Data.Aeson              as Json
import GHC.Generics
import Control.Monad.Except
import qualified Data.Char               as C
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL


-------------------------- Maelstrom Server -------------------------------------------------

data MaelstromContext = NotInitialized | Initialized NodeData

data NodeData = NodeData {
  nodeId  :: Text,
  nodeIds :: [Text]
}

type ClientHandler = NodeData -> Event -> Response
type MaelstromHandler = MaelstromMessage -> ExceptT String (State MaelstromContext) MaelstromMessage

runMaelstrom :: StateT MaelstromContext IO () -> MaelstromContext -> IO ()
runMaelstrom maelstromServer context = 
  do
    (_, newContext) <- runStateT maelstromServer context
    runMaelstrom maelstromServer newContext
    
createMaelstromServer :: ClientHandler -> StateT MaelstromContext IO ()
createMaelstromServer clientHandler =
  do
    let maelstromHandler = wrapperClientHandler clientHandler

    line <- lift getLine
    responseMessageOrError <-
        liftState 
      $ runExceptT
      $ maelstromHandler =<< (liftEither $ (eitherDecodeMessage line))
    
    lift $ log' ("Received: " ++ line)

    case responseMessageOrError of
      Left e -> lift $ log' e
      Right responseMessage -> lift $ send responseMessage

wrapperClientHandler :: ClientHandler -> MaelstromHandler
wrapperClientHandler clientHandler message =
  ExceptT $ do
    let header = getHeader message
    maelstromContext <- get
    
    case parseToEvents message of
      Left e -> return $ throwError e

      Right event ->
        case maelstromContext of
          NotInitialized ->
            
            case event of
              InitEvent _msgId _nodeId _nodeIds ->
                do
                  put $ Initialized NodeData { nodeId = _nodeId, nodeIds = _nodeIds }
                  return $ return $ parseFromResponse header (InitOkResponse _msgId)
              
              EchoEvent _msgId _ ->
                return $ return $ parseFromResponse header (ErrorReponse _msgId 11 "Not Initialized")
    
          Initialized nodeData -> 
            return $ return $ parseFromResponse header (clientHandler nodeData event)

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

---------------------------------------------------------------------------------------------

------------------------- Client Events and Responses ---------------------------------------

data Header = Header { srcHeader :: Text, destHeader :: Text }

data Event =
    InitEvent { msgIdEvent :: Int, nodeIdEvent :: Text, nodeIdsEvent :: [Text] }
  | EchoEvent { msgIdEvent :: Int, echoEvent :: Text }

data Response =
    InitOkResponse { inReplyToResp :: Int }  
  | EchoOkResponse { msgIdResp :: Int, inReplyToResp :: Int, echoResp :: Text }
  | ErrorReponse   { inReplyToResp :: Int, codeResp :: Int, textResp :: Text }

getHeader :: MaelstromMessage -> Header
getHeader message = Header (src message) (dest message)

parseToEvents :: MaelstromMessage -> Either String Event
parseToEvents message =
  case body message of
    Init _msgId _nodeId _nodeIds -> Right $ InitEvent _msgId _nodeId _nodeIds
    
    Init_Ok _                    -> Left "Received an init_ok message"
    
    Error _ _ _                  -> Left "Received an error message"
    
    Echo _msgId _echo            -> Right $ EchoEvent _msgId _echo
    
    Echo_Ok _ _ _                -> Left "Received an echo_ok message"

parseFromResponse :: Header -> Response -> MaelstromMessage
parseFromResponse header reponse =
  case reponse of
    InitOkResponse _inReplyTo  -> 
      MaelstromMessage source destination (Init_Ok _inReplyTo)
    
    EchoOkResponse _msgId _inReplyTo _echo -> 
      MaelstromMessage source destination (Echo_Ok _msgId _inReplyTo _echo)
    
    ErrorReponse _inReplyTo _code _text ->
      MaelstromMessage source destination (Error _inReplyTo _code _text)
  where
    source = srcHeader header
    destination = srcHeader header

---------------------------------------------------------------------------------------------

