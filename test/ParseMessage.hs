{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module ParseMessage (htf_thisModulesTests) where

import Test.Framework
import MyLib (Message(..), Body(..))
import Data.Aeson
import qualified Data.ByteString.Lazy as BL

test_parseInit :: IO ()
test_parseInit =
  let message = Message {
    src = "Joe",
    dest = "12",
    body = Init { msg_id = 1, node_id = "Nothing", node_ids = ["Nothing"] }
  } 
  in assertEqual (Just message) (decodeMessage . encodeMessage $ message)

test_parseInit_Ok :: IO ()
test_parseInit_Ok =
  let message = Message {
    src = "Joe",
    dest = "12",
    body = Init_Ok { in_reply_to = 1 }
  } 
  in assertEqual (Just message) (decodeMessage . encodeMessage $ message)

test_parseEcho :: IO ()
test_parseEcho =
  let message = Message {
    src = "Joe",
    dest = "12",
    body = Echo { msg_id = 1, echo = "Nothing" }
  } 
  in assertEqual (Just message) (decodeMessage . encodeMessage $ message)

test_parseEcho_Ok :: IO ()
test_parseEcho_Ok =
  let message = Message {
    src = "Joe",
    dest = "12",
    body = Echo_Ok { msg_id = 1, in_reply_to = 1, echo = "Nothing" }
  } 
  in assertEqual (Just message) (decodeMessage . encodeMessage $ message)

encodeMessage :: Message -> BL.ByteString
encodeMessage = encode

decodeMessage :: BL.ByteString -> Maybe Message
decodeMessage = decode