module Main where

import MaelstromServer

main :: IO ()
main = runMaelstrom (createMaelstromServer myHandler) NotInitialized
  
myHandler :: NodeData -> MaelstromMessage -> MaelstromMessage
myHandler _ = id