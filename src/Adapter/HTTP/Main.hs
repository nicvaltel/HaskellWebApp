{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.HTTP.Main where


import Reexport
import ClassyPrelude (headMay)

import Network.Wai
import Domain.Auth (AuthRepo, EmailVerificationNotif, SessionRepo)

import qualified Adapter.HTTP.API.Main as API
import qualified Adapter.HTTP.Web.Main as Web
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Vhost


main :: 
  (MonadUnliftIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m) =>
  Int -> (m Response -> IO Response) -> IO ()
main port runner = do
  web <- Web.main runner
  api <- API.main runner
  run port $ vhost [(pathBeginsWith "api", api)] web
  where
    pathBeginsWith path req = headMay (pathInfo req) == Just path
