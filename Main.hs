module Main where

import Control.Concurrent.STM
import Data.IntMap
import Yesod

import Dispatch()
import Foundation

main :: IO ()
main = do
  --tstore <- atomically $ newTVar empty
  tstore <- atomically $ newTVar empty
  tfilenames <- atomically $ newTVar []
  warpEnv $ App tfilenames

