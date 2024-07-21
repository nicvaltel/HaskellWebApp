{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.HTTP.Web.Main where


import Reexport
import Domain.Auth
import ClassyPrelude
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import qualified Adapter.HTTP.API.Auth as Auth
import Adapter.HTTP.API.Common
import Katip
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

  get "/" $
    text "Hello from web!"

  notFound $ do
    status status404
    text "Not found"

  defaultHandler $ Handler $ \(e :: SomeException) -> do
    lift $ $(logTM) ErrorS $ "Unhandeled error: " <> ls (show e)
    status status500
    text "InternalServerError"
    
