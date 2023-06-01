{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module ParseMessage (htf_thisModulesTests) where

import Test.Framework
import MyLib (Message(..), Body(..))
import Data.Aeson
import qualified Data.ByteString.Lazy as BL

test_1 :: IO ()
test_1 =
  let message = Message {
    src = "Joe",
    dest = "12",
    body = Body { type_text = "type", msg_id = (Just 1), in_reply_to = Nothing}
  } 
  in assertEqual (Just message) (decodeMessage . encodeMessage $ message)

encodeMessage :: Message -> BL.ByteString
encodeMessage = encode

decodeMessage :: BL.ByteString -> Maybe Message
decodeMessage = decode