{-# LANGUAGE OverloadedStrings #-}
module Main where

import MaelstromServer
import qualified Data.Text as T

main :: IO ()
main = runMaelstrom (createMaelstromServer myHandler) NotInitialized
  
myHandler :: NodeData -> Header -> Event -> Response    
myHandler _ (Header _src _dest) (GenerateEvent _msgId) = 
    GenerateOkResponse _msgId _msgId (_dest <> _src <> (T.pack $ show _msgId))

myHandler _ _ event = ErrorReponse (getMsgId event) 12 "Alredy Initialized"
