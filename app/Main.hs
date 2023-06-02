module Main where

import System.IO (hPutStrLn, stderr)
import qualified MyLib
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Aeson
import Control.Monad

main :: IO ()
main = forever $ do
  line <- getLine
  hPutStrLn stderr ("Received: " ++ line)
  case (eitherDecode  . TL.encodeUtf8 .TL.pack) line of 
    Left e        -> hPutStrLn stderr e
    Right message ->
      let response = (encodeMessage . MyLib.handler) message
      in do 
        hPutStrLn stderr ("Transmited: " ++ response)
        putStrLn response

encodeMessage :: MyLib.Message -> String
encodeMessage = TL.unpack . TL.decodeUtf8 . encode