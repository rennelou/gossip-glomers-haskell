module Main where

import MaelstromServer

main :: IO ()
main = runMaelstrom myHandler
  
myHandler :: NodeData -> Message -> Message
myHandler _ = id