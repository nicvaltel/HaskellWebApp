{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.HTTP.Web.Auth where


import Reexport hiding (concatMap)
import Web.Scotty.Trans
import Domain.Auth
import Text.Digestive.Scotty
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.View as DF
import Text.Digestive.Form ((.:))
import Adapter.HTTP.Common
import Adapter.HTTP.Web.Common
import Text.Blaze.Html5 ((!))
import qualified Text.Digestive.Blaze.Html5 as DH
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import ClassyPrelude (concatMap)

-- * Routest

routes :: (MonadUnliftIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m) => ScottyT m ()
routes = do
  -- home
  get "/" $
    redirect "/users"

  -- register
  get "/auth/register" $ do
    view <- DF.getForm "auth" authForm
    renderHtml $ registerPage view []
  
  post "/auth/register" $ do
    (view, mayAuth) <- runForm "auth" authForm
    case mayAuth of 
      Nothing -> renderHtml $ registerPage view []
      Just auth -> do
        result <- lift $ register auth
        case result of
          Left RegistrationErrorEmailTaken -> renderHtml $ registerPage view ["Email has been taken"]
          Right _ -> do
            v <- DF.getForm "auth" authForm
            renderHtml $ registerPage v ["Registered successfully"]

  -- verify email
  get "/auth/verifyEmail/:code" $ do
    code <- captureParam "code" `catch` (\(_ :: SomeException) -> pure "")
    result <- lift $ verifyEmail code
    case result of
      Left EmailVerificationErrorInvalidCode -> 
        renderHtml $ verifyEmailPage "The verification code is invalid"
      Right _ ->
        renderHtml $ verifyEmailPage "Your email has been verified"

  -- login
  get "/auth/login" $ do
    view <- DF.getForm "auth" authForm
    renderHtml $ loginPage view []

  post "/auth/login" $ do
    (view, mayAuth) <- runForm "auth" authForm
    case mayAuth of
      Nothing -> renderHtml $ loginPage view []
      Just auth -> do
        result <- lift $ login auth
        case result of
          Left LoginErrorEmailNotVerified -> 
            renderHtml $ loginPage view ["Email has not been verified"]
          Left LoginErrorInvalidAuth ->
            renderHtml $ loginPage view ["Email/password is incorrect"]
          Right sId -> do
            setSessionIdInCookie sId
            redirect "/"

  -- get user
  get "/users" $ do
    userId <- reqCurrentUserId
    mayEmail <- lift $ getUser userId
    case mayEmail of
      Nothing -> throwString "Should not happen: email is not found"
      Just email -> renderHtml $ userPage (rawEmail email)

userPage :: Text -> H.Html
userPage email =
  mainLayout "Users" $ do
    H.div $
      H.h1 "Users"
    H.div $
      H.toHtml email

verifyEmailPage :: Text -> H.Html
verifyEmailPage msg =
  mainLayout "Email Verification" $ do
    H.h1 "Email Verififcation"
    H.div $ H.toHtml msg
    H.div $ H.a ! A.href "/auth/login" $ "Login"

authForm :: Monad m => DF.Form [Text] m Auth
authForm = do
  Auth <$> "email" .: emailForm
       <*> "password" .: passwordForm
  where
    emailForm = DF.validate (toResult . mkEmail) (DF.text Nothing)
    passwordForm = DF.validate (toResult . mkPassword) (DF.text Nothing)

registerPage :: DF.View [Text] -> [Text] -> H.Html
registerPage view msgs =
  mainLayout "Register" $ do
    H.div $
      authFormLayout view "Register111!!!" "/auth/register" msgs
    H.div $
      H.a ! A.href "/auth/login" $ "Login"

authFormLayout :: DF.View [Text] -> Text -> Text -> [Text] -> H.Html
authFormLayout view formTitle action msgs =
  formLayout view action $ do
    H.h2 $
      H.toHtml formTitle
    H.div $
      errorList msgs
    H.div $ do
      H.label "Email"
      DH.inputText "email" view
      H.div $
        errorList' "email"
    H.div $ do
      H.label "Password"
      DH.inputPassword "password" view
      H.div $
        errorList' "password"
    H.input ! A.type_ "submit" ! A.value "Submit"
  where
    errorList' path =
      errorList . mconcat $ DF.errors path view 
    errorList =
      H.ul . concatMap errorItem
    errorItem =
      H.li . H.toHtml

loginPage :: DF.View [Text] -> [Text] -> H.Html
loginPage view msgs =
  mainLayout "Login" $ do
    H.div $
      authFormLayout view "Login" "/auth/login" msgs
    H.div $
      H.a ! A.href "/auth/register" $ "Register"