module Main where

import qualified MyLib
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Aeson
import Control.Monad

main :: IO ()
main = forever $ do
  line <- getLine
  (putStrLn . show . MyLib.handler . tryDecodeMessage) line

tryDecodeMessage :: String -> MyLib.Message
tryDecodeMessage s =
  case (eitherDecode  . TL.encodeUtf8 .TL.pack) s
  of Left e        -> error e
     Right message -> message
