module Main where

import MaelstromServer

main :: IO ()
main = runMaelstrom (wrapperHandler myHandler) NotInitialized
  
myHandler :: NodeData -> MaelstromMessage -> MaelstromMessage
myHandler _ = id