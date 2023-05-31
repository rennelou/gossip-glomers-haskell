{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import MyLib (Message(..), Body(..))
import Data.Aeson
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  putStrLn $ 
    "Encode: "
    ++
    ( show $ decodeMessage $ encodeMessage
      ( Message 
        { src = "Joe",
          dest = "12",
          body = Body { type_text = "type", msg_id = (Just 1), in_reply_to = Nothing}
        })
    )


encodeMessage :: Message -> BL.ByteString
encodeMessage = encode

decodeMessage :: BL.ByteString -> Maybe Message
decodeMessage = decode