{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module ParseMessage (htf_thisModulesTests) where

import Test.Framework
import MyLib (Message(..), Body(..))
import Data.Aeson
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL

test_parseInit :: IO ()
test_parseInit =
  let jsonMessage = "{\
        \\"src\":\"c1\"\
        \,\"dest\":\"n1\"\
        \,\"body\":{\
        \  \"type\":\"init\"\
        \ ,\"msg_id\":1\
        \ ,\"node_id\":\"n3\"\
        \ ,\"node_ids\":[\"n1\",\"n2\",\"n3\"]}\
        \}"
      message = Message {
        src = "c1",
        dest = "n1",
        body = Init { msg_id = 1, node_id = "n3", node_ids = ["n1", "n2", "n3"]}
      }
  in assertEqual message (strDecodeMessage jsonMessage)

test_parseInit_Ok :: IO ()
test_parseInit_Ok =
  let message = Message {
    src = "Joe",
    dest = "12",
    body = Init_Ok { in_reply_to = 1 }
  } 
  in assertEqual message (decodeMessage . encodeMessage $ message)

test_parseEcho :: IO ()
test_parseEcho =
  let message = Message {
    src = "Joe",
    dest = "12",
    body = Echo { msg_id = 1, echo = "Nothing" }
  } 
  in assertEqual message (decodeMessage . encodeMessage $ message)

test_parseEcho_Ok :: IO ()
test_parseEcho_Ok =
  let message = Message {
    src = "Joe",
    dest = "12",
    body = Echo_Ok { msg_id = 1, in_reply_to = 1, echo = "Nothing" }
  } 
  in assertEqual message (decodeMessage . encodeMessage $ message)

encodeMessage :: Message -> BL.ByteString
encodeMessage = encode

decodeMessage :: BL.ByteString -> Message
decodeMessage b = 
  case eitherDecode b of
    Left e -> error e
    Right message -> message

strEncodeMessage :: Message -> String
strEncodeMessage = TL.unpack . TL.decodeUtf8 . encode

strDecodeMessage :: String -> Message
strDecodeMessage s =  
  case (eitherDecode . TL.encodeUtf8 .TL.pack) s of
    Left e -> error e
    Right message -> message