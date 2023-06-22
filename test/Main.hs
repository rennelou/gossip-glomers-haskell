{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main ( main ) where

import Test.Framework
-- Import modules defining HTF tests like this:
import {-@ HTF_TESTS @-} ParseMaelstromMessage

main :: IO ()
main = htfMain htf_importedTests
