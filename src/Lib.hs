{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib (runRoutine) where

import Reexport
import Domain.Auth
import qualified Adapter.InMemory.Auth as Mem
import qualified Adapter.PostgreSQL.Auth as PG
import qualified Adapter.Redis.Auth as RDS
import qualified Configuration.Dotenv as Dotenv
import qualified Adapter.RabbitMQ.Common as MQ
import qualified Adapter.RabbitMQ.Auth as MQAuth
import qualified Data.Text as T

import Katip (KatipContextT)
import Logging (withKatip)
import qualified Data.ByteString.Char8 as BSC8
import qualified Adapter.HTTP.Main as HTTP



type AppState = (PG.State, RDS.State, MQ.State, TVar Mem.State) -- Try change Mem.MemState to TVar Mem.State

newtype App a = App { unApp :: ReaderT AppState (KatipContextT IO) a  } 
  deriving (Functor, Applicative, Monad, MonadReader AppState, MonadIO, MonadUnliftIO, MonadFail, MonadThrow, MonadCatch, KatipContext, Katip)


instance AuthRepo App where
  addAuth = PG.addAuth
  setEmailAsVerified = PG.setEmailAsVerified
  findUserByAuth = PG.findUserByAuth
  findEmailFromUserId = PG.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = MQAuth.notifyEmailVerification

instance SessionRepo App where
  newSession = RDS.newSession
  findUserIdBySessionId = RDS.findUserIdBySessionId




runState :: LogEnv -> AppState -> App a -> IO a
runState le state =
  runKatipContextT le () mempty 
  . flip runReaderT state 
  . unApp
  
runState' :: LogEnv -> b -> ReaderT b (KatipContextT m) a -> m a
runState' le state =
  runKatipContextT le () mempty 
  . flip runReaderT state 
  
withState :: (Int -> LogEnv -> AppState -> IO ()) -> IO()
withState action = do
  pgCfg <- either error id <$> readDBConfig "db/database.env"
  redisCfg <- either error id <$> readRedisConfig "redis/database.env"
  let port = 3000

  withKatip $ \le -> do 
    memState <- newTVarIO Mem.initialState
    PG.withState pgCfg $ \pgState ->
      RDS.withState redisCfg $ \redisState ->
        MQ.withState mqCfg 16 $ \mqState -> do
          let appState = (pgState, redisState, mqState, memState) 
          action port le appState 

    where
    mqCfg = "amqp://guest:guest@localhost:5672/%2F"

    readRedisConfig :: String -> IO (Either String String)
    readRedisConfig file = do
      env <- Dotenv.parseFile file
      let result :: Either String String = do
            dbHost <- maybeToRight "No Hostname defined" (lookup "REDIS_HOST" env)
            dbPort :: Int <- maybeToRight "No port number defined" (read <$> lookup "REDIS_PORT" env)
            dbPassword <- maybeToRight "No password defined" (lookup "REDIS_PASSWORD" env)
            dbSelectDb <- maybeToRight "No select db defined" (lookup "REDIS_SELECT_DB" env)
            let configUrl :: String  = printf "redis://%s:%d/%s" dbHost dbPort dbSelectDb 
            pure configUrl
      pure result

    readDBConfig :: String -> IO (Either String PG.Config)
    readDBConfig file = do
      env <- Dotenv.parseFile file
      let result :: Either String PG.Config = do
            dbHost <- maybeToRight "No Hostname defined" (lookup "POSTGRES_HOST" env)
            dbPort :: Int <- maybeToRight "No port number defined" (read <$> lookup "POSTGRES_PORT" env)
            dbName <- maybeToRight "No database name defined" (lookup "POSTGRES_DB" env)
            dbUser <- maybeToRight "No username defined" (lookup "POSTGRES_USER" env)
            dbPassword <- maybeToRight "No password defined" (lookup "POSTGRES_PASSWORD" env)
            configStripeCount <- maybeToRight "No stripe count defined" (read <$> lookup "POSTGRES_STRIPE_COUNT" env)
            dbMaxOpenConnPerStripe <- maybeToRight "No max open connections per stripe defined" (read <$> lookup "POSTGRES_MAX_OPEN_CONN_PER_STRIPE" env)
            dbIdleConnTimeout <- maybeToRight "No stripe count defined" (read <$> lookup "POSTGRES_IDLE_CONN_TIMEOUT" env)
            let configUrl :: String  = printf "postgresql://%s:%s@%s:%d/%s" dbUser dbPassword dbHost dbPort dbName 
            pure PG.Config {PG.configUrl = BSC8.pack configUrl, PG.configStripeCount = configStripeCount, PG.configMaxOpenConnPerStripe = dbMaxOpenConnPerStripe, PG.congigIdleConnTimeout = dbIdleConnTimeout}
      pure result


runRoutine :: IO ()
runRoutine = do
  withState $ \port le appState@(_, _, mqState, _) -> do
    let runner = runState le appState
    MQAuth.init mqState runner
    HTTP.main port runner


routine :: App ()
routine = do
  emailFileContent <- liftIO $ T.pack <$> readFile "test-email.cfg"
  liftIO $ putStrLn emailFileContent
  let email = either undefined id $ mkEmail emailFileContent
  let passw = either undefined id $ mkPassword "123456Hello"
  let auth = Auth email passw
  _ <- register auth
  vCode <- App $ pollNotif email
  -- Just vCode <- App $ Mem.getNotificationsForEmail email
  _ <- verifyEmail vCode
  Right session <- login auth
  Just uId <- resolveSessionId session
  Just registeredEmail <- getUser uId
  liftIO $ print (session, uId, registeredEmail)
  where
    pollNotif email = do
      result <- Mem.getNotificationsForEmail email
      case result of
        Nothing -> pollNotif email
        Just vCode -> return vCode



