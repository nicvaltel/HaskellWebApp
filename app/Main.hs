module Main (main) where

import Prelude(IO)
import qualified WebSocketServer as W
import qualified Lib
import qualified Logging

main :: IO ()
main = do
  Lib.runRoutine
  -- Logging.runKatip
  -- W.main