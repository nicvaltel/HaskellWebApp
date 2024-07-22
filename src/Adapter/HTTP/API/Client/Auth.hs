{-# LANGUAGE FlexibleContexts #-}
module Adapter.HTTP.API.Client.Auth where


import Reexport
import Network.HTTP.Client
import qualified Domain.Auth as D
import Network.HTTP.Types
import Adapter.HTTP.API.Types.Auth ()
import Adapter.HTTP.API.Client.Common


register :: HTTPClient r m => D.Auth -> m (Either D.RegistrationError ())
register auth = do
  State initReq mgr <- asks getter
  let req = initReq {
      method = "POST"
    , path = "/api/auth/register"
    , requestBody = RequestBodyLBS $ encode auth
  }
  resp <- liftIO $ httpLbs req mgr
  case responseStatus resp of
    (Status 200 _) -> pure $ Right ()
    _ -> Left <$> parseOnErr req resp