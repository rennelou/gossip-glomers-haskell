module Main where

import MaelstromServer

main :: IO ()
main = runMaelstrom myHandler
  
myHandler :: NodeData -> MaelstromMessage -> MaelstromMessage
myHandler _ = id