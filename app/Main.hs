module Main where

import qualified MyLib
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Aeson
import Control.Monad

main :: IO ()
main = forever $ do
  line <- getLine
  putStrLn . show $ (fmap MyLib.handler $ decodeRawMessage line)

decodeRawMessage :: String -> Maybe MyLib.Message
decodeRawMessage = decode . TL.encodeUtf8 .TL.pack
