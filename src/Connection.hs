module Connection where

import Reexport
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close)

openAndClose = do
  conn <- connectPostgreSQL "postgresql://username:pass@localhost:6666/quorumdb"
  putStrLn "OKAY"
  close conn