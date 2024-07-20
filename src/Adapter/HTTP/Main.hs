{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.HTTP.Main where


import Reexport
import Web.Scotty.Trans
import ClassyPrelude (LText, MonadUnliftIO, SomeException (SomeException))
import Web.Scotty.Trans (ScottyException)
import qualified Data.Text.Lazy as T
import Network.HTTP.Types
import qualified Adapter.HTTP.API.Auth as AuthAPI
import Adapter.HTTP.Common

import Katip
import Network.Wai
import Network.Wai.Middleware.Gzip
import Domain.Auth (AuthRepo, EmailVerificationNotif, SessionRepo)


main :: 
  (MonadUnliftIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m) =>
  Int -> (m Response -> IO Response) -> IO ()
main port runner = scottyT port runner routes

routes :: 
  ( MonadUnliftIO m, KatipContext m, AuthRepo m , EmailVerificationNotif m, SessionRepo m) =>
  ScottyT m ()
routes = do
  middleware $ gzip $ def {gzipFiles = GzipCompress}

  AuthAPI.routes

  defaultHandler $ Handler $ \(e :: SomeException) -> do
    lift $ $(logTM) ErrorS $ "Unhandeled error: " <> ls (show e)
    status status500
    json ("InternalServerError" :: Text)
    
