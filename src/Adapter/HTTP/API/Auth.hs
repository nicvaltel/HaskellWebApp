module Adapter.HTTP.API.Auth where

import Reexport
import Web.Scotty.Trans
import Domain.Auth
import qualified Text.Digestive.Form as DF
import Text.Digestive.Form ((.:))
import Adapter.HTTP.Common
import Network.HTTP.Types.Status
import Adapter.HTTP.API.Common
import Adapter.HTTP.API.Types.Auth -- need for ophran instances


authForm :: Monad m => DF.Form [Text] m Auth
authForm = do
  Auth <$> "email" .: emailForm
       <*> "password" .: passwordForm
  where
    emailForm = DF.validate (toResult . mkEmail) (DF.text Nothing)
    passwordForm = DF.validate (toResult . mkPassword) (DF.text Nothing)


verifyEmailForm :: Monad m => DF.Form [Text] m VerificationCode
verifyEmailForm = DF.text Nothing


routes :: (MonadUnliftIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m) => ScottyT m ()
routes = do
  -- register
  post "/api/auth/register" $ do
    input <- parseAndValidateJSON authForm
    domainResult <- lift $ register input
    case domainResult of
      Left err -> do
        status status400
        json err
      Right _ -> pure ()

  -- verify email
  post "/api/auth/verifyEmail" $ do
    input <- parseAndValidateJSON verifyEmailForm
    domainResult <- lift $ verifyEmail input
    case domainResult of
      Left err -> do
        status status400
        json err
      Right _ -> pure ()

  -- login 
  post "/api/auth/login" $ do
    input <- parseAndValidateJSON authForm
    domainResult <- lift $ login input
    case domainResult of
      Left err -> do
        status status400
        json err
      Right sId -> do
        setSessionIdInCookie sId
        pure ()

  -- get user
  get "/api/users" $ do
    userId <- reqCurrentUserId
    mayEmail <- lift $ getUser userId
    case mayEmail of
      Nothing -> throwString "Should not happen: SessionId map to invalid UserId"
      Just email -> json $ rawEmail email

