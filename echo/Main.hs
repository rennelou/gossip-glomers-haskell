{-# LANGUAGE OverloadedStrings #-}
module Main where

import MaelstromServer

main :: IO ()
main = runMaelstrom (createMaelstromServer myHandler) NotInitialized
  
myHandler :: NodeData -> Event -> Response
myHandler _ (InitEvent _msgId _nodeId _nodeIds) = ErrorReponse _msgId 12 "Alredy Initialized"
    
myHandler _ (EchoEvent _msgId _echo) = EchoOkResponse _msgId _msgId _echo
