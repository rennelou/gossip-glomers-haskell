{-# LANGUAGE DeriveGeneric #-}
module MaelstromServer (
    MaelstromMessage(..)
  , Body(..)
  , NodeData(..)
  , MaelstromContext(NotInitialized)
  , Header(..)
  , Event(..)
  , Response(..)
  , runMaelstrom
  , createMaelstromServer
  , getMsgId ) where

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

type ClientHandler = NodeData -> Header -> Event -> Response
type MaelstromHandler = MaelstromMessage -> ExceptState MaelstromContext MaelstromMessage

runMaelstrom :: MaelstromHandler -> MaelstromContext -> IO ()
runMaelstrom maelstromHandler context = 
  do
    line <- getLine
    
    log' ("Received: " ++ line)

    let (responseOrError, newContext) = runExceptState 
                                          (     maelstromHandler 
                                            =<< (liftEither $ eitherDecodeMessage line) )
                                          context

    case responseOrError of
      Left e -> do log' e
      Right responseMessage -> do send responseMessage

    runMaelstrom maelstromHandler newContext

createMaelstromServer :: ClientHandler -> MaelstromHandler
createMaelstromServer clientHandler message =
  do
    let header = getHeader message
    event <- liftEither $ parseToEvents message
    maelstromContext <- lift get

    case maelstromContext of
      NotInitialized ->
        
        case event of
          InitEvent _msgId _nodeId _nodeIds ->
            do
              lift $ put $ Initialized NodeData { nodeId = _nodeId, nodeIds = _nodeIds }
              return $ parseFromResponse header (InitOkResponse _msgId)
          
          _ ->
            return $ parseFromResponse header (ErrorReponse (getMsgId event) 11 "Not Initialized")

      Initialized nodeData -> 
        return $ parseFromResponse header (clientHandler nodeData header event)

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
eitherDecodeMessage = Json.eitherDecode . TL.encodeUtf8 . TL.pack

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
    Init        { msg_id :: Int, node_id :: Text, node_ids :: [Text] }
  | Init_Ok     { in_reply_to :: Int }
  | Error       { in_reply_to :: Int, code :: Int, text :: Text }
  | Echo        { msg_id :: Int, echo :: Text }
  | Echo_Ok     { msg_id :: Int, in_reply_to :: Int, echo :: Text }
  | Generate    { msg_id :: Int }
  | Generate_Ok { msg_id :: Int, in_reply_to :: Int, id :: Text }
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

data Header = Header { srcHeader :: Text, destHeader :: Text }

data Event =
    InitEvent     { msgIdEvent :: Int, nodeIdEvent :: Text, nodeIdsEvent :: [Text] }
  | EchoEvent     { msgIdEvent :: Int, echoEvent :: Text }
  | GenerateEvent { msgIdEvent :: Int }

data Response =
    InitOkResponse     { inReplyToResp :: Int }  
  | EchoOkResponse     { msgIdResp :: Int, inReplyToResp :: Int, echoResp :: Text }
  | GenerateOkResponse { msgIdResp :: Int, inReplyToResp :: Int, idResp :: Text }
  | ErrorReponse       { inReplyToResp :: Int, codeResp :: Int, textResp :: Text }

getHeader :: MaelstromMessage -> Header
getHeader message = Header (src message) (dest message)

getMsgId :: Event -> Int
getMsgId event =
  case event of
    InitEvent _msgId _ _ -> _msgId
          
    EchoEvent _msgId _   -> _msgId

    GenerateEvent _msgId -> _msgId

parseToEvents :: MaelstromMessage -> Either String Event
parseToEvents message =
  case body message of
    Init _msgId _nodeId _nodeIds -> Right $ InitEvent _msgId _nodeId _nodeIds
    
    Init_Ok _                    -> Left "Received an init_ok message"
    
    Error _ _ _                  -> Left "Received an error message"
    
    Echo _msgId _echo            -> Right $ EchoEvent _msgId _echo
    
    Echo_Ok _ _ _                -> Left "Received an echo_ok message"

    Generate _msgId              -> Right $ GenerateEvent _msgId
    
    Generate_Ok _ _ _            -> Left "Received an generate_ok message"

parseFromResponse :: Header -> Response -> MaelstromMessage
parseFromResponse header reponse =
  case reponse of
    InitOkResponse _inReplyTo  -> 
      MaelstromMessage source destination (Init_Ok _inReplyTo)
    
    EchoOkResponse _msgId _inReplyTo _echo -> 
      MaelstromMessage source destination (Echo_Ok _msgId _inReplyTo _echo)
    
    GenerateOkResponse _msgId _inReplyTo _id ->
      MaelstromMessage source destination (Generate_Ok _msgId _inReplyTo _id)
    
    ErrorReponse _inReplyTo _code _text ->
      MaelstromMessage source destination (Error _inReplyTo _code _text)
  where
    source = destHeader header
    destination = srcHeader header

---------------------------------------------------------------------------------------------

