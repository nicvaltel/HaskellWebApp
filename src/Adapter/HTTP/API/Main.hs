{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.HTTP.API.Main where


import Reexport
import Domain.Auth
import ClassyPrelude
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import qualified Adapter.HTTP.API.Auth as Auth
import Adapter.HTTP.API.Common
import Network.Wai
import Network.Wai.Middleware.Gzip



main :: 
  (MonadUnliftIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m) =>
  (m Response -> IO Response) -> IO Application
main runner = scottyAppT defaultOptions runner routes

routes :: 
  ( MonadUnliftIO m, KatipContext m, AuthRepo m , EmailVerificationNotif m, SessionRepo m) =>
  ScottyT m ()
routes = do
  middleware $ gzip $ def {gzipFiles = GzipCompress}

  Auth.routes

  notFound $ do
    status status404
    json $ errorResponce ("NotFound" :: Text)

  defaultHandler $ Handler $ \(e :: SomeException) -> do
    lift $ $(logTM) ErrorS $ "Unhandeled error: " <> ls (show e)
    status status500
    json ("InternalServerError" :: Text)
    
