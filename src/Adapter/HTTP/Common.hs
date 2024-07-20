{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.HTTP.Common where

import Reexport
import Web.Scotty.Trans
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Types as DF
import qualified Text.Digestive.Aeson as DF
import Text.Digestive.Form ((.:))

import Data.Aeson hiding (json, (.:))
import Network.HTTP.Types.Status
import Blaze.ByteString.Builder (toLazyByteString)
import Web.Cookie
import Data.Text.Lazy (toStrict)
import ClassyPrelude (decodeUtf8, Utf8 (encodeUtf8))
import Domain.Auth
import Data.Time.Lens


toResult :: Either e a -> DF.Result e a
toResult = either DF.Error DF.Success


parseAndValidateJSON :: (MonadUnliftIO m, ToJSON v) => DF.Form v m a -> ActionT m a
parseAndValidateJSON form = do
  val <- jsonData `catch` (\(_:: SomeException) -> pure Null)
  validataionResult <- lift $ DF.digestJSON form val
  case validataionResult of
    (v, Nothing) -> do
      status status400
      json $ DF.jsonErrors v
      finish
    (_, Just result) -> pure result


setCookie :: MonadIO m => SetCookie -> ActionT m ()
setCookie = setHeader "Set-Cookie" . decodeUtf8 . toLazyByteString . renderSetCookie


getCookie :: Monad m => Text -> ActionT m (Maybe Text)
getCookie key = do
  mCookieStr <- header "Cookie"
  pure $ do
    cookie <- parseCookies . encodeUtf8 . toStrict <$> mCookieStr
    let bsKey = encodeUtf8 key
    val <- lookup bsKey cookie
    pure $ decodeUtf8 val


setSessionIdInCookie :: MonadIO m => SessionId -> ActionT m ()
setSessionIdInCookie sId = do
  curTime <- liftIO getCurrentTime
  setCookie $ def{
      setCookieName = "sId"
    , setCookiePath = Just "/"
    , setCookieValue = encodeUtf8 sId
    , setCookieExpires = Just $ modL month (+ 1) curTime
    , setCookieHttpOnly = True
    , setCookieSecure = False
    , setCookieSameSite = Just sameSiteLax
    }

getCurrentUserId :: SessionRepo m => ActionT m (Maybe UserId)
getCurrentUserId = do
  maySessionId <- getCookie "sId"
  case maySessionId of
    Nothing -> pure Nothing
    Just sId -> lift $ resolveSessionId sId

reqCurrentUserId :: (SessionRepo m, MonadIO m) => ActionT m UserId
reqCurrentUserId = do
  mayUser <- getCurrentUserId
  case mayUser of
    Nothing -> do
      status status401
      json ("AuthRequired" :: Text)
      finish
    Just userId ->
      pure userId
