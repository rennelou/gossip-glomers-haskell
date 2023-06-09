{-# LANGUAGE OverloadedStrings #-}
module Main where

import MaelstromServer

main :: IO ()
main = runMaelstrom (createMaelstromServer myHandler) NotInitialized
  
myHandler :: NodeData -> Header -> Event -> Response
myHandler _ _ (EchoEvent _msgId _echo) = EchoOkResponse _msgId _msgId _echo

myHandler _ _ event = ErrorReponse (getMsgId event) 12 "Alredy Initialized"
