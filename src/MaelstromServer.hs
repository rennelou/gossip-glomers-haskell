{-# LANGUAGE DeriveGeneric #-}
module MaelstromServer (
    MaelstromMessage(..)
  , Body(..)
  , NodeData(..)
  , MaelstromContext(NotInitialized)
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

type ClientHandler = NodeData -> MaelstromMessage -> MaelstromMessage
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
    responseOrError <-
        liftState 
      $ runExceptT
      $ maelstromHandler =<< (liftEither $ eitherDecodeMessage line)
    
    lift $ log' ("Received: " ++ line)

    case responseOrError of
      Left e -> lift $ log' e
      Right responseMessage -> lift $ send responseMessage

wrapperClientHandler :: ClientHandler -> MaelstromHandler
wrapperClientHandler clientHandler message =
  ExceptT $ do
    maelstromContext <- get
    case maelstromContext of
      NotInitialized ->
        
        case body message of
          Init _msgId _nodeId _nodeIds ->
            do
              put $ Initialized NodeData { nodeId = _nodeId, nodeIds = _nodeIds }
              return $ return $ initOkMsg message _msgId

          Init_Ok _ ->
            return $ throwError "Received an init_ok message"
          
          Error _ _ _ ->
            return $ throwError "Received an error message"
          
          Echo _msgId _ ->
            return $ return $ notInitializedErrorMsg message _msgId
          
          Echo_Ok _ _ _ ->
            return $ throwError "Received an echo_ok message"

      Initialized nodeData -> return $ return $ clientHandler nodeData message

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

-- * Insert Events, Responses and Parses Here *

---------------------------------------------------------------------------------------------

